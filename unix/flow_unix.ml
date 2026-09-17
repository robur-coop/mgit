let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let ( let* ) = Result.bind

type ctx = unit
type t = Miou_unix.file_descr

let resolve host port =
  let hints = Unix.[ AI_SOCKTYPE SOCK_STREAM ] in
  match Unix.getaddrinfo host (string_of_int port) hints with
  | [] -> error_msgf "%s:%d: no address" host port
  | infos -> Ok (List.map (fun { Unix.ai_addr; _ } -> ai_addr) infos)
  | exception Unix.Unix_error (err, _, _) ->
      error_msgf "%s: %s" host (Unix.error_message err)

let socket = function
  | Unix.ADDR_INET (inet, _) when Unix.is_inet6_addr inet -> Miou_unix.tcpv6 ()
  | _ -> Miou_unix.tcpv4 ()

let rec attempt = function
  | [] -> error_msgf "Connection refused"
  | sockaddr :: rest ->
      let fd = socket sockaddr in
      begin match Miou_unix.connect fd sockaddr with
      | () -> Ok fd
      | exception Unix.Unix_error (err, _, _) ->
          Miou_unix.close fd;
          begin match rest with
          | [] -> error_msgf "Connection failed: %s" (Unix.error_message err)
          | rest -> attempt rest
          end
      end

let connect () edn =
  match edn.Mgit.Endpoint.scheme with
  | `Git ->
      let host = edn.Mgit.Endpoint.host and port = Mgit.Endpoint.port edn in
      let* targets = resolve host port in
      attempt targets
  | `SSH | `HTTP | `HTTPS ->
      error_msgf "Unsupported scheme: %a" Mgit.Endpoint.pp edn

let recv t buf ~off ~len =
  match Miou_unix.read t ~off ~len buf with
  | 0 -> Ok `End
  | len -> Ok (`Len len)
  | exception Unix.Unix_error (err, _, _) ->
      error_msgf "recv: %s" (Unix.error_message err)

let send t str ~off ~len =
  match Miou_unix.write t ~off ~len str with
  | () -> Ok len
  | exception Unix.Unix_error (err, _, _) ->
      error_msgf "send: %s" (Unix.error_message err)

let close t = try Miou_unix.close t with Unix.Unix_error _ -> ()
