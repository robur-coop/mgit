(* NOTE(dinosaure): from mfetch (extended) *)

let src = Logs.Src.create "mgit.smart"
let ( let* ) = Protocol.bind

module Log = (val Logs.src_log src : Logs.LOG)

type error =
  [ Protocol.error
  | `No_branch
  | `Invalid_version of string
  | `No_side_band
  | `Err of string ]

let pp_error ppf = function
  | #Protocol.error as err -> Protocol.pp_error ppf err
  | `No_branch -> Fmt.string ppf "No branch available"
  | `Invalid_version v -> Fmt.pf ppf "Invalid Smart version: %S" v
  | `No_side_band ->
      Fmt.string ppf "The remote does not support the side-band capability"
  | `Err msg -> Fmt.pf ppf "Remote error: %s" msg

type refs = {
  refs : (string * string) list (* refname, oid (hex) *);
  peeled : (string * string) list;
      (* refname, oid (hex) of the object an annotated tag points at *)
  head : Carton.Uid.t; (* oid (hex) *)
  head_symref : string option (* refs/heads/main *);
}

type advertisement =
  | V1 of { refs : refs; capabilities : string list }
  | V2 of { capabilities : string list }

let attribute ~prefix attrs =
  let fn attr =
    if String.starts_with ~prefix attr
    then
      let off = String.length prefix
      and len = String.length attr - String.length prefix in
      Some (String.sub attr off len)
    else None in
  List.find_map fn attrs

let err_of_pkt pkt = attribute ~prefix:"ERR " [ pkt ]
let uid_of_hex hex = Carton.Uid.unsafe_of_string (Ohex.decode hex)

let head_of_refs refs head_symref =
  match (List.assoc_opt "HEAD" refs, head_symref) with
  | Some head, _ when head <> "unborn" -> Protocol.return (uid_of_hex head)
  | _, Some symref ->
      begin match List.assoc_opt symref refs with
      | Some head -> Protocol.return (uid_of_hex head)
      | None -> Protocol.error `No_branch
      end
  | _ -> Protocol.error `No_branch

let ref_of_line line =
  match String.split_on_char ' ' (String.trim line) with
  | [ oid; name ] -> Some (name, oid)
  | _ -> None

let advertisement_v1 first ctx =
  let first, capabilities =
    match String.index_opt first '\000' with
    | Some idx ->
        ( String.sub first 0 idx,
          String.sub first (idx + 1) (String.length first - idx - 1) )
    | None -> (first, "") in
  let capabilities =
    List.filter (( <> ) "")
      (String.split_on_char ' ' (String.trim capabilities)) in
  let head_symref = attribute ~prefix:"symref=HEAD:" capabilities in
  let rec go acc ctx =
    let* pkt = Protocol.decode_pkt ctx in
    match String.trim pkt with
    | "" -> Protocol.return (List.rev acc)
    | line ->
        begin match ref_of_line line with
        | Some value -> go (value :: acc) ctx
        | None -> Protocol.error `Invalid_pkt_line
        end in
  match (err_of_pkt first, ref_of_line first) with
  | Some msg, _ -> Protocol.error (`Err msg)
  | None, None -> Protocol.error `Invalid_pkt_line
  | None, Some value ->
      let* advertised = go [ value ] ctx in
      let fn (refs, peeled) (name, oid) =
        if name = "capabilities^{}"
        then (refs, peeled)
        else if String.ends_with ~suffix:"^{}" name
        then
          let name = String.sub name 0 (String.length name - 3) in
          (refs, (name, oid) :: peeled)
        else ((name, oid) :: refs, peeled) in
      let refs, peeled = List.fold_left fn ([], []) advertised in
      let refs = List.rev refs and peeled = List.rev peeled in
      let* head = head_of_refs refs head_symref in
      Protocol.return
        (V1 { refs = { refs; peeled; head; head_symref }; capabilities })

(* NOTE(dinosaure): for [git-daemon] *)
let proto_request ?(version = 2) ~service ~host path ctx =
  let extra = if version >= 2 then Fmt.str "\000version=%d\000" version else "" in
  Protocol.encode_pkt ctx "%s %s\000host=%s\000%s" service path host extra

let advertisement ctx =
  let rec version ctx =
    let* pkt = Protocol.decode_pkt ctx in
    match String.trim pkt with
    | "" -> version ctx
    | pkt when pkt.[0] = '#' -> version ctx (* NOTE(dinosaure): for HTTP *)
    | pkt -> Protocol.return pkt in
  let* pkt = version ctx in
  match String.split_on_char ' ' pkt with
  | [ "version"; "2" ] ->
      let rec capabilities acc ctx =
        let* pkt = Protocol.decode_pkt ctx in
        match String.trim pkt with
        | "" -> Protocol.return (List.rev acc)
        | capability -> capabilities (capability :: acc) ctx in
      let* capabilities = capabilities [] ctx in
      Protocol.return (V2 { capabilities })
  | [ "version"; ("0" | "1") ] ->
      let* pkt = Protocol.decode_pkt ctx in
      advertisement_v1 (String.trim pkt) ctx
  | [ "version"; v ] -> Protocol.error (`Invalid_version v)
  | _ -> advertisement_v1 pkt ctx

(* NOTE(dinosaure): we need to split [ls_refs] and [fetch] for HTTP. *)

let ls_refs ctx =
  let* () = Protocol.encode_pkt ctx "command=ls-refs\n" in
  let* () = Protocol.encode_pkt ctx "object-format=sha1" in
  let* () = Protocol.encode_delim_pkt ctx in
  let* () = Protocol.encode_pkt ctx "symrefs" in
  let* () = Protocol.encode_pkt ctx "peel" in
  (* NOTE(dinosaure): filter references. *)
  let* () = Protocol.encode_pkt ctx "ref-prefix HEAD" in
  let* () = Protocol.encode_pkt ctx "ref-prefix refs/heads/" in
  let* () = Protocol.encode_pkt ctx "ref-prefix refs/tags/" in
  let* () = Protocol.encode_flush_pkt ctx in
  let rec go acc peeled head_symref ctx =
    let* pkt = Protocol.decode_pkt ctx in
    match String.trim pkt with
    | "" -> Protocol.return (List.rev acc, List.rev peeled, head_symref)
    | line ->
        begin match String.split_on_char ' ' line with
        | oid :: name :: attrs ->
            let head_symref =
              if name = "HEAD"
              then
                match attribute ~prefix:"symref-target:" attrs with
                | Some _ as value -> value
                | None -> head_symref
              else head_symref in
            let peeled =
              match attribute ~prefix:"peeled:" attrs with
              | Some oid -> (name, oid) :: peeled
              | None -> peeled in
            go ((name, oid) :: acc) peeled head_symref ctx
        | _ -> Protocol.error `Invalid_pkt_line
        end in
  let* refs, peeled, head_symref = go [] [] None ctx in
  let* head = head_of_refs refs head_symref in
  Protocol.return { refs; peeled; head; head_symref }

let rec side_band errored q ctx =
  let* pkt = Protocol.decode_pkt ctx in
  if String.length pkt = 0
  then Protocol.return errored
  else
    let data = String.sub pkt 1 (String.length pkt - 1) in
    match pkt.[0] with
    | '\001' ->
        Flux.Bqueue.put q data ;
        side_band errored q ctx
    | '\003' ->
        Log.err (fun m -> m "[remote]: %s" data) ;
        side_band true q ctx
    | _ -> side_band errored q ctx

let rec iter fn = function
  | [] -> Protocol.return ()
  | x :: rest -> let* () = fn x in iter fn rest

let hex (uid : Carton.Uid.t) = Ohex.encode (uid :> string)

let uid_of_hex_opt str =
  match Ohex.decode str with
  | uid when String.length uid = 20 -> Some (Carton.Uid.unsafe_of_string uid)
  | _ -> None
  | exception _ -> None

type shallow_update = [ `Shallow of Carton.Uid.t | `Unshallow of Carton.Uid.t ]

let shallow_list ctx =
  let rec go acc =
    let* packet = Protocol.decode_pkt_or_delim_or_end ctx in
    match packet with
    | `Flush | `Delim | `End -> Protocol.return (List.rev acc)
    | `Line line ->
        let line = String.trim line in
        begin match String.split_on_char ' ' line with
        | [ "shallow"; value ] ->
            begin match uid_of_hex_opt value with
            | Some uid -> go (`Shallow uid :: acc)
            | None -> Protocol.error `Invalid_pkt_line
            end
        | [ "unshallow"; value ] ->
            begin match uid_of_hex_opt value with
            | Some uid -> go (`Unshallow uid :: acc)
            | None -> Protocol.error `Invalid_pkt_line
            end
        | _ ->
            begin match err_of_pkt line with
            | Some msg -> Protocol.error (`Err msg)
            | None ->
                Log.err (fun m -> m "Expected shallow/unshallow, got %S" line);
                Protocol.error `Invalid_pkt_line
            end
        end in
  go []

type ack =
  [ `NAK
  | `ACK of Carton.Uid.t
  | `ACK_continue of Carton.Uid.t
  | `ACK_common of Carton.Uid.t
  | `ACK_ready of Carton.Uid.t ]

let contains ~sub str =
  let n = String.length sub and m = String.length str in
  let rec go i = i + n <= m && (String.sub str i n = sub || go (i + 1)) in
  go 0

let get_ack ctx =
  let* packet = Protocol.decode_pkt_or_delim_or_end ctx in
  match packet with
  | `Flush | `Delim | `End ->
      Log.err (fun m -> m "Expected ACK/NAK, got a flush packet");
      Protocol.error `Invalid_pkt_line
  | `Line line ->
      let line = String.trim line in
      if line = "NAK" then Protocol.return `NAK
      else if String.starts_with ~prefix:"ACK " line && String.length line >= 44
      then
        match uid_of_hex_opt (String.sub line 4 40) with
        | None -> Protocol.error `Invalid_pkt_line
        | Some uid ->
            let rest = String.sub line 44 (String.length line - 44) in
            if String.trim rest = "" then Protocol.return (`ACK uid)
            else if contains ~sub:"continue" rest then Protocol.return (`ACK_continue uid)
            else if contains ~sub:"common" rest then Protocol.return (`ACK_common uid)
            else if contains ~sub:"ready" rest then Protocol.return (`ACK_ready uid)
            else Protocol.return (`ACK uid)
      else
        match err_of_pkt line with
        | Some msg -> Protocol.error (`Err msg)
        | None ->
            Log.err (fun m -> m "Expected ACK/NAK, got %S" line);
            Protocol.error `Invalid_pkt_line
