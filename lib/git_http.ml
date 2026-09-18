module type CLIENT = sig
  type ctx

  val request :
       ctx
    -> meth:[ `GET | `POST ]
    -> headers:(string * string) list
    -> ?body:string Seq.t
    -> uri:string
    -> (string -> unit)
    -> (unit, [> `Msg of string ]) result
end

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let inhibit fn = try fn () with _exn -> ()

module Make (Client : CLIENT) = struct
  type ctx = Client.ctx

  type request =
    { body : (string, string option) Flux.Bqueue.t option
    ; response : (string, string option) Flux.Bqueue.t
    ; prm : (unit, [ `Msg of string ]) result Miou.t
    ; discovery : bool
    ; mutable rem : string
    ; mutable reading : bool
    ; mutable result : (unit, [ `Msg of string ]) result option }

  type t =
    { ctx : Client.ctx
    ; base : string
    ; service : string
    ; headers : (string * string) list
    ; mutable current : request option
    ; sniff : Buffer.t
    ; state : Buffer.t
    ; mutable state_done : bool }

  let connect ctx edn ~service ~version =
    let headers =
      if version >= 2 && service = "git-upload-pack"
      then [ ("Git-Protocol", Fmt.str "version=%d" version) ]
      else [] in
    Ok
      { ctx
      ; base= Endpoint.uri edn
      ; service
      ; headers
      ; current= None
      ; sniff= Buffer.create 64
      ; state= Buffer.create 0x100
      ; state_done= false }

  let start t ~meth ~headers ~uri ~with_body =
    let response = Flux.Bqueue.(create with_close_and_halt) 0x100 in
    let body = if with_body then Some (Flux.Bqueue.(create with_close) 0x100) else None in
    let prm =
      Miou.async @@ fun () ->
      let finally () = inhibit (fun () -> Flux.Bqueue.close response) in
      Fun.protect ~finally @@ fun () ->
      let fn str = inhibit (fun () -> Flux.Bqueue.put response str) in
      let body = Option.map Flux.Bqueue.to_seq body in
      match Client.request t.ctx ~meth ~headers ?body ~uri fn with
      | Ok () -> Ok ()
      | Error (`Msg _ as err) -> Error err in
    { body; response; prm; discovery= meth = `GET; rem= ""; reading= false
    ; result= None }

  let stop req =
    Option.iter (fun q -> inhibit (fun () -> Flux.Bqueue.close q)) req.body;
    inhibit (fun () -> Flux.Bqueue.halt req.response);
    if Option.is_none req.result then
      let result = match Miou.await req.prm with
        | Ok result -> result
        | Error exn -> error_msgf "%s" (Printexc.to_string exn) in
      req.result <- Some result

  let finish t = Option.iter stop t.current; t.current <- None

  let result req =
    match req.result with
    | Some result -> result
    | None ->
        let result =
          match Miou.await req.prm with
          | Ok result -> result
          | Error exn -> error_msgf "%s" (Printexc.to_string exn) in
        req.result <- Some result;
        result

  let is_v2 t =
    let str = Buffer.contents t.sniff in
    let sub = "version 2\n" in
    let n = String.length sub in
    let rec go i = i + n <= String.length str && (String.sub str i n = sub || go (i + 1)) in
    go 0

  let scan t =
    let str = Buffer.contents t.state in
    let rec go pos =
      if pos + 4 > String.length str then ()
      else
        match int_of_string_opt ("0x" ^ String.sub str pos 4) with
        | Some 0 ->
            Buffer.truncate t.state (pos + 4);
            t.state_done <- true
        | Some len when len >= 4 -> go (pos + len)
        | _ -> go (pos + 4) in
    go 0

  let post t =
    let uri = Fmt.str "%s/%s" t.base t.service in
    let headers =
      ("Content-Type", Fmt.str "application/x-%s-request" t.service)
      :: ("Accept", Fmt.str "application/x-%s-result" t.service)
      :: t.headers in
    let replay = Option.is_some t.current && t.state_done && not (is_v2 t) in
    finish t;
    let req = start t ~meth:`POST ~headers ~uri ~with_body:true in
    if replay then
      Option.iter (fun q -> Flux.Bqueue.put q (Buffer.contents t.state)) req.body;
    t.current <- Some req;
    req

  let send t str ~off ~len =
    let req =
      match t.current with
      | Some ({ discovery= false; reading= false; _ } as req) -> req
      | Some _ | None -> post t in
    let str = String.sub str off len in
    if not t.state_done then begin
      Buffer.add_string t.state str;
      scan t
    end;
    begin match req.body with
    | Some q -> Flux.Bqueue.put q str
    | None -> ()
    end;
    Ok len

  let discover t =
    let uri = Fmt.str "%s/info/refs?service=%s" t.base t.service in
    let req = start t ~meth:`GET ~headers:t.headers ~uri ~with_body:false in
    t.current <- Some req;
    req

  let rec next req =
    if req.rem <> "" then Some req.rem
    else if Option.is_some req.result then None
    else
      match Flux.Bqueue.get req.response with
      | Some "" -> next req
      | Some _ as value -> value
      | None -> None

  let recv t buf ~off ~len =
    let req = match t.current with Some req -> req | None -> discover t in
    if not req.reading then begin
      Option.iter Flux.Bqueue.close req.body;
      req.reading <- true
    end;
    match next req with
    | Some str ->
        let len = Int.min len (String.length str) in
        Bytes.blit_string str 0 buf off len;
        if req.discovery && Buffer.length t.sniff < 64 then
          Buffer.add_string t.sniff
            (String.sub str 0 (Int.min len (64 - Buffer.length t.sniff)));
        req.rem <- String.sub str len (String.length str - len);
        Ok (`Len len)
    | None ->
        begin match result req with
        | Ok () -> Ok `End
        | Error (`Msg msg) -> finish t; Error (`Msg msg)
        end

  let close t = finish t
end
