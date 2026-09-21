let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let inhibit fn value = try fn value with _exn -> ()
let ( let* ) = Result.bind

type ctx =
  { happy_eyeballs : Mnet_happy_eyeballs.t
  ; ssh : [ `Pubkey of Awa.Hostkey.priv | `Password of string ] option
  ; ssh_authenticator : Awa.Keys.authenticator option
  ; authenticator : X509.Authenticator.t option (* for HTTPS *) }

let ctx ?ssh ?ssh_authenticator ?authenticator happy_eyeballs =
  { happy_eyeballs; ssh; ssh_authenticator; authenticator }

module Client = struct
  type nonrec ctx = ctx

  let request ctx ~meth ~headers ?body ~uri fn =
    let body = Option.map Mhttp_client.stream body in
    let handler _meta _req resp () = function
      | Some str when Mhttp_client.Status.is_successful resp.Mhttp_client.status -> fn str
      | _ -> () in
    let meth = (meth :> H1.Method.t) in
    match
      Mhttp_client.request ?authenticator:ctx.authenticator ~follow_redirect:true
        ~meth ~headers ?body ~happy_eyeballs:ctx.happy_eyeballs ~fn:handler ~uri ()
    with
    | Ok (resp, ()) when Mhttp_client.Status.is_successful resp.Mhttp_client.status ->
        Ok ()
    | Ok (resp, ()) ->
        error_msgf "%s: %a" uri Mhttp_client.Status.pp_hum resp.Mhttp_client.status
    | Error err -> error_msgf "%s: %a" uri Mhttp_client.pp_error err
end

module Http = Mgit_http.Make (Client)

type tcp =
  { flow : Mnet.TCP.direct Mnet.TCP.flow
  ; mutable pending : string list
  ; mutable off : int (* NOTE(dinosaure): into the first pending string *) }

type flow = Tcp of tcp | Ssh of Mnet_ssh.flow | Http of Http.t
type t = { flow : flow; resource : Miou.Ownership.t }

let owned ~finally value =
  let resource = Miou.Ownership.create ~finally value in
  Miou.Ownership.own resource;
  resource

let quote str =
  let buf = Buffer.create (String.length str + 2) in
  Buffer.add_char buf '\'';
  String.iter
    (function '\'' -> Buffer.add_string buf "'\\''" | chr -> Buffer.add_char buf chr)
    str;
  Buffer.add_char buf '\'';
  Buffer.contents buf

let connect ctx edn ~service ~version =
  let host = edn.Mgit_sync.Endpoint.host and port = Mgit_sync.Endpoint.port edn in
  match edn.Mgit_sync.Endpoint.scheme with
  | `Git ->
      let kind = Mnet.TCP.direct in
      let* _, flow = Mnet_happy_eyeballs.connect ~kind ctx.happy_eyeballs host [ port ] in
      let finally = inhibit Mnet.TCP.close in
      let resource = owned ~finally flow in
      Ok { flow= Tcp { flow; pending= []; off= 0 }; resource }
  | `SSH ->
      begin match ctx.ssh with
      | None -> error_msgf "%a: no SSH credential" Mgit_sync.Endpoint.pp edn
      | Some credential ->
          let user = Option.value ~default:"git" edn.Mgit_sync.Endpoint.user in
          let command = Fmt.str "%s %s" service (quote edn.Mgit_sync.Endpoint.path) in
          let kind = Mnet.TCP.direct in
          let* _, flow = Mnet_happy_eyeballs.connect ~kind ctx.happy_eyeballs host [ port ] in
          let finally = inhibit Mnet.TCP.close in
          let resource = owned ~finally flow in
          let authenticator = ctx.ssh_authenticator in
          let result = Mnet_ssh.client ?authenticator ~user credential
            command flow in
          begin match result with
          | Ok ssh -> Ok { flow= Ssh ssh; resource }
          | Error _ as err ->
              Miou.Ownership.release resource;
              err
          end
      end
  | `HTTP | `HTTPS ->
      let* http = Http.connect ctx edn ~service ~version in
      Ok { flow= Http http; resource= owned ~finally:Http.abort http }

let rec recv_tcp tcp buf ~off ~len =
  match tcp.pending with
  | str :: rest ->
      let n = Int.min len (String.length str - tcp.off) in
      Bytes.blit_string str tcp.off buf off n;
      if tcp.off + n >= String.length str
      then (tcp.pending <- rest; tcp.off <- 0)
      else tcp.off <- tcp.off + n;
      if n = 0 then recv_tcp tcp buf ~off ~len else Ok (`Len n)
  | [] ->
      begin match Mnet.TCP.read tcp.flow with
      | Ok strs ->
          tcp.pending <- List.filter (( <> ) "") strs;
          recv_tcp tcp buf ~off ~len
      | Error `Eof -> Ok `End
      | Error `Refused -> error_msgf "Connection refused"
      | exception Mnet.TCP.Closed_by_peer -> Ok `End
      | exception Mnet.TCP.Net_unreach -> error_msgf "Network unreachable"
      end

let recv t buf ~off ~len =
  match t.flow with
  | Tcp tcp -> recv_tcp tcp buf ~off ~len
  | Ssh flow ->
      begin match Mnet_ssh.read flow buf ~off ~len with
      | 0 -> Ok `End
      | len -> Ok (`Len len)
      | exception exn -> error_msgf "ssh: %s" (Printexc.to_string exn)
      end
  | Http http -> Http.recv http buf ~off ~len

let send t str ~off ~len =
  match t.flow with
  | Tcp { flow; _ } ->
      begin match Mnet.TCP.write flow ~off ~len str with
      | () -> Ok len
      | exception Mnet.TCP.Closed_by_peer -> error_msgf "Connection closed by peer"
      | exception Mnet.TCP.Net_unreach -> error_msgf "Network unreachable"
      end
  | Ssh flow ->
      begin match Mnet_ssh.write flow str ~off ~len with
      | () -> Ok len
      | exception exn -> error_msgf "ssh: %s" (Printexc.to_string exn)
      end
  | Http http -> Http.send http str ~off ~len

let close t =
  begin match t.flow with
  | Tcp { flow; _ } -> inhibit Mnet.TCP.close flow
  | Ssh flow -> inhibit Mnet_ssh.close flow
  | Http http -> Http.close http
  end;
  Miou.Ownership.disown t.resource
