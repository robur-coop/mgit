let run env body =
  let input = Filename.temp_file "git-http" ".in" in
  let output = Filename.temp_file "git-http" ".out" in
  let finally () = Sys.remove input; Sys.remove output in
  Fun.protect ~finally @@ fun () ->
  Out_channel.with_open_bin input (fun oc -> Out_channel.output_string oc body);
  let stdin = Unix.openfile input [ Unix.O_RDONLY ] 0 in
  let stdout = Unix.openfile output Unix.[ O_WRONLY; O_TRUNC ] 0 in
  let pid = Unix.create_process_env "git" [| "git"; "http-backend" |] env stdin stdout Unix.stderr in
  Unix.close stdin;
  Unix.close stdout;
  ignore (Unix.waitpid [] pid);
  In_channel.with_open_bin output In_channel.input_all

let split str =
  let sep, len = match Str.search_forward (Str.regexp_string "\r\n\r\n") str 0 with
    | idx -> (idx, 4)
    | exception Not_found ->
      begin match Str.search_forward (Str.regexp_string "\n\n") str 0 with
      | idx -> (idx, 2)
      | exception Not_found -> String.length str, 0 end in
  let headers = String.sub str 0 sep in
  let payload = String.sub str (sep + len) (String.length str - sep - len) in
  let fn (status, headers) line =
    let line = String.trim line in
    match String.index_opt line ':' with
    | None -> (status, headers)
    | Some idx ->
      let key = String.sub line 0 idx in
      let value = String.sub line (idx + 1) (String.length line - idx - 1) in
      let value = String.trim value in
      if String.lowercase_ascii key = "status"
      then let[@warning "-8"] word :: _ = String.split_on_char ' ' value in
           (int_of_string word, headers)
      else (status, (key, value) :: headers) in
  let status, headers = List.fold_left fn (200, []) (String.split_on_char '\n' headers) in
  (status, List.rev headers, payload)

let git root reqd body =
  let open H1 in
  let request = Reqd.request reqd in
  let meth = Method.to_string request.Request.meth in
  let target = request.Request.target in
  let path, query = match String.index_opt target '?' with
    | Some idx ->
      let path = String.sub target 0 idx in
      let query = String.sub target (idx+1) (String.length target - idx - 1) in
      (path, query)
    | None -> (target, "") in
  let header name = Headers.get request.Request.headers name in
  let protocol = match Sys.getenv_opt "GIT_HTTP_V0" with
    | Some _ -> None
    | None -> header "git-protocol" in
  let env =
    [ "GIT_PROJECT_ROOT=" ^ root
    ; "GIT_HTTP_EXPORT_ALL=1"
    ; "REQUEST_METHOD=" ^ meth
    ; "PATH_INFO=" ^ path
    ; "QUERY_STRING=" ^query
    ; "REMOTE_ADDR=127.0.0.1"
    ; "REMOTE_USER=mgit"
    ; "CONTENT_TYPE=" ^ Option.value ~default:"" (header "content-type")
    ; "CONTENT_LENGTH=" ^ string_of_int (String.length body) ] in
  let env = match protocol with
    | None -> env
    | Some protocol ->
      ("HTTP_GIT_PROTOCOL=" ^ protocol)
      :: ("GIT_PROTOCOL=" ^ protocol)
      :: env in
  let env = Array.append (Unix.environment ()) (Array.of_list env) in
  let status, headers, payload = split (run env body) in
  let headers = ("content-length", string_of_int (String.length payload)) :: headers in
  let headers = Headers.of_list headers in
  let response = Response.create ~headers (Status.of_code status) in
  Reqd.respond_with_string reqd response payload

let[@warning "-8"] handler root _flow _conn (`V1 reqd : Httpcats.Server.reqd) =
  let body = H1.Reqd.request_body reqd in
  let buf = Buffer.create 0x7ff in
  let rec on_eof () = H1.Body.Reader.close body; git root reqd (Buffer.contents buf)
  and on_read bstr ~off ~len =
    Buffer.add_string buf (Bstr.sub_string bstr ~off ~len);
    H1.Body.Reader.schedule_read body ~on_eof ~on_read in
  H1.Body.Reader.schedule_read body ~on_eof ~on_read

let ( / ) = Filename.concat

let () = Miou_unix.run ~domains:0 @@ fun () ->
  let root, address, port = match Sys.argv with
    | [| _; dir; port; addr |] ->
      let root =
        if Filename.is_relative dir
        then (Sys.getcwd () / dir) else dir in
      let address = Unix.inet_addr_of_string addr in
      let port = int_of_string port in
      (root, address, port)
    | [| _; dir; port; |] ->
      let root =
        if Filename.is_relative dir
        then (Sys.getcwd () / dir) else dir in
      let address = Unix.inet_addr_loopback in
      let port = int_of_string port in
      (root, address, port)
    | _ -> Format.eprintf "%s <dir> <port> [<addr>]\n%!" Sys.executable_name;
           exit 1 in
  let sockaddr = Unix.ADDR_INET (address, port) in
  let socket = Miou_unix.tcpv4 () in
  Unix.setsockopt (Miou_unix.to_file_descr socket) Unix.SO_REUSEADDR true;
  Miou_unix.bind_and_listen socket sockaddr;
  let ready = Miou.Computation.create () in
  let handler = handler root in
  let server = Miou.async @@ fun () ->
    Httpcats.Server.clear ~parallel:false ~ready ~handler
      (Httpcats.Server.Use (socket, sockaddr)) in
  Miou.Computation.await_exn ready;
  close_out (open_out "git-http-ok");
  Miou.await_exn server;
  Miou_unix.close socket
