let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let ( let* ) = Result.bind
let inhibit fn value = try fn value with _exn -> ()

type ctx =
  { ssh : string
  ; authenticator : X509.Authenticator.t option }

let ctx ?(ssh = "ssh") ?authenticator () = { ssh; authenticator }

(* TCP/IP *)

let resolve host port =
  let hints = Unix.[ AI_SOCKTYPE SOCK_STREAM ] in
  match Unix.getaddrinfo host (string_of_int port) hints with
  | [] -> error_msgf "%s:%d: no address" host port
  | infos -> Ok (List.map (fun { Unix.ai_addr; _ } -> ai_addr) infos)
  | exception Unix.Unix_error (err, _, _) ->
      error_msgf "%s: %s" host (Unix.error_message err)

let owned ~finally value =
  let resource = Miou.Ownership.create ~finally value in
  Miou.Ownership.own resource;
  resource

let socket = function
  | Unix.ADDR_INET (inet, _) when Unix.is_inet6_addr inet -> Miou_unix.tcpv6 ()
  | _ -> Miou_unix.tcpv4 ()

let rec attempt = function
  | [] -> error_msgf "Connection refused"
  | sockaddr :: rest ->
      let fd = socket sockaddr in
      let finally fd = inhibit Unix.close (Miou_unix.to_file_descr fd) in
      let resource = owned ~finally fd in
      begin match Miou_unix.connect fd sockaddr with
      | () -> Ok (fd, resource)
      | exception Unix.Unix_error (err, _, _) ->
          Miou_unix.close fd;
          Miou.Ownership.disown resource;
          begin match rest with
          | [] -> error_msgf "Connection failed: %s" (Unix.error_message err)
          | rest -> attempt rest
          end
      end

(* SSH *)

type process =
  { pid : int
  ; stdin : Miou_unix.file_descr
  ; stdout : Miou_unix.file_descr }

let quote str =
  let buf = Buffer.create (String.length str + 2) in
  Buffer.add_char buf '\'';
  String.iter
    (function '\'' -> Buffer.add_string buf "'\\''" | chr -> Buffer.add_char buf chr)
    str;
  Buffer.add_char buf '\'';
  Buffer.contents buf

(* The process is killed (and reaped) if the task which spawned it ends
   abnormally. *)
let abort { pid; stdin; stdout } =
  inhibit Unix.close (Miou_unix.to_file_descr stdin);
  inhibit Unix.close (Miou_unix.to_file_descr stdout);
  inhibit (fun () -> Unix.kill pid Sys.sigkill ) ();
  inhibit (fun () -> ignore (Unix.waitpid [] pid)) ()

let spawn prog args env =
  let in_r, in_w = Unix.pipe ~cloexec:true () in
  let out_r, out_w = Unix.pipe ~cloexec:true () in
  match
    Unix.create_process_env prog (Array.of_list (prog :: args)) env in_r out_w
      Unix.stderr
  with
  | pid ->
      Unix.close in_r;
      Unix.close out_w;
      let process =
        { pid
        ; stdin= Miou_unix.of_file_descr ~non_blocking:true in_w
        ; stdout= Miou_unix.of_file_descr ~non_blocking:true out_r } in
      Ok (process, owned ~finally:abort process)
  | exception Unix.Unix_error (err, _, _) ->
      List.iter Unix.close [ in_r; in_w; out_r; out_w ];
      error_msgf "%s: %s" prog (Unix.error_message err)

let ssh ctx edn ~service ~version =
  let host =
    match edn.Mgit_sync.Endpoint.user with
    | Some user -> user ^ "@" ^ edn.Mgit_sync.Endpoint.host
    | None -> edn.Mgit_sync.Endpoint.host in
  let port =
    match edn.Mgit_sync.Endpoint.port with
    | Some port -> [ "-p"; string_of_int port ]
    | None -> [] in
  (* NOTE(dinosaure): the version of the protocol is given to the remote as
     [GIT_PROTOCOL], if the SSH server accepts it. *)
  let env, send_env =
    if version >= 2
    then
      ( Array.append (Unix.environment ()) [| Fmt.str "GIT_PROTOCOL=version=%d" version |]
      , [ "-o"; "SendEnv=GIT_PROTOCOL" ] )
    else (Unix.environment (), []) in
  let command = Fmt.str "%s %s" service (quote edn.Mgit_sync.Endpoint.path) in
  spawn ctx.ssh (send_env @ port @ [ host; command ]) env

(* HTTP *)

module Client = struct
  type nonrec ctx = ctx

  let request ctx ~meth ~headers ?body ~uri fn =
    let body = Option.map Httpcats.stream body in
    let handler _meta _req resp () = function
      | Some str when Httpcats.Status.is_successful resp.Httpcats.status -> fn str
      | _ -> () in
    let meth = (meth :> H1.Method.t) in
    match
      Httpcats.request ?authenticator:ctx.authenticator ~follow_redirect:true
        ~meth ~headers ?body ~fn:handler ~uri ()
    with
    | Ok (resp, ()) when Httpcats.Status.is_successful resp.Httpcats.status -> Ok ()
    | Ok (resp, ()) ->
        error_msgf "%s: %a" uri Httpcats.Status.pp_hum resp.Httpcats.status
    | Error err -> error_msgf "%s: %a" uri Httpcats.pp_error err
end

module Http = Mgit_http.Make (Client)

(* Git_flow *)

type flow = Tcp of Miou_unix.file_descr | Ssh of process | Http of Http.t
type t = { flow : flow; resource : Miou.Ownership.t }

let connect ctx edn ~service ~version =
  match edn.Mgit_sync.Endpoint.scheme with
  | `Git ->
      let host = edn.Mgit_sync.Endpoint.host
      and port = Mgit_sync.Endpoint.port edn in
      let* targets = resolve host port in
      let* fd, resource = attempt targets in
      Ok { flow= Tcp fd; resource }
  | `SSH ->
      let* process, resource = ssh ctx edn ~service ~version in
      Ok { flow= Ssh process; resource }
  | `HTTP | `HTTPS ->
      let* http = Http.connect ctx edn ~service ~version in
      Ok { flow= Http http; resource= owned ~finally:Http.abort http }

let read fd buf ~off ~len =
  match Miou_unix.read fd ~off ~len buf with
  | 0 -> Ok `End
  | len -> Ok (`Len len)
  | exception Unix.Unix_error (err, _, _) ->
      error_msgf "recv: %s" (Unix.error_message err)

let write fd str ~off ~len =
  match Miou_unix.write fd ~off ~len str with
  | () -> Ok len
  | exception Unix.Unix_error (err, _, _) ->
      error_msgf "send: %s" (Unix.error_message err)

let recv t buf ~off ~len =
  match t.flow with
  | Tcp fd -> read fd buf ~off ~len
  | Ssh { stdout; _ } -> read stdout buf ~off ~len
  | Http http -> Http.recv http buf ~off ~len

let send t str ~off ~len =
  match t.flow with
  | Tcp fd -> write fd str ~off ~len
  | Ssh { stdin; _ } -> write stdin str ~off ~len
  | Http http -> Http.send http str ~off ~len

let close_fd fd = try Miou_unix.close fd with Unix.Unix_error _ -> ()

let close t =
  begin match t.flow with
  | Tcp fd -> close_fd fd
  | Ssh { pid; stdin; stdout } ->
      close_fd stdin;
      close_fd stdout;
      (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ())
  | Http http -> Http.close http
  end;
  Miou.Ownership.disown t.resource
