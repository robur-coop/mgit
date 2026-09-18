let src = Logs.Src.create "mgit.store"

module Log = (val Logs.src_log src : Logs.LOG)
module SHA1 = Digestif.SHA1

type error =
  [ `Msg of string
  | `Zone_full
  | `Not_found of Carton.Uid.t ]

let pp_error ppf = function
  | `Msg msg -> Fmt.string ppf msg
  | `Zone_full -> Fmt.string ppf "Zone of the block-device full"
  | `Not_found uid -> Fmt.pf ppf "%a not found" Git_object.pp_uid uid

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let ref_length = Git_object.ref_length

exception Out_of_space = Blk.Append.Out_of_space

type metadata =
  { hdr_len : int
  ; pack_off : int
  ; pack_len : int
  ; idx_off : int
  ; idx_len : int }

let rd bstr =
  let get n = Int64.to_int (Bstr.get_int64_le bstr (n * 8)) in
  match (get 0, get 1, get 2, get 3, get 4) with
  | hdr_len, pack_off, pack_len, idx_off, idx_len
    when hdr_len >= 0 && pack_off >= 0 && pack_len >= 0 && idx_off >= 0
         && idx_len >= 0 ->
      Ok { hdr_len; pack_off; pack_len; idx_off; idx_len }
  | _ -> Error `Invalid_metadata

let wr m bstr =
  let set n v = Bstr.set_int64_le bstr (n * 8) (Int64.of_int v) in
  set 0 m.hdr_len;
  set 1 m.pack_off;
  set 2 m.pack_len;
  set 3 m.idx_off;
  set 4 m.idx_len;
  5 * 8

module Make (Block : Blk.BLOCK) = struct
  module Blk = Blk.Make (Block)

  type fd = metadata Blk.t

  type t =
    { blk : metadata Blk.t
    ; header : Bundle.t option
    ; idx : fd Classeur.t option
    ; carton : fd Carton.t option }

  let allocate bits = De.make_window ~bits

  let header_of blk =
    let m = Blk.metadata blk in
    if m.hdr_len = 0 then None
    else
      let seq = Blk.seq blk `Active ~off:0 ~len:m.hdr_len () in
      match Bundle.split ~ref_length seq with
      | Ok (hdr, _rest) -> Some hdr
      | Error (`Msg msg) ->
          Log.err (fun m -> m "Invalid bundle header: %s" msg);
          None

  let idx_of blk =
    let m = Blk.metadata blk in
    if m.idx_len = 0 then None
    else
      let cache = Blk.cachet blk `Active ~base:m.idx_off ~len:m.idx_len in
      Some
        (Classeur.of_cachet ~length:m.idx_len ~hash_length:SHA1.digest_size
           ~ref_length cache)

  let carton_of blk idx =
    let m = Blk.metadata blk in
    if m.pack_len = 0 then None
    else
      let cache = Blk.cachet blk `Active ~base:m.pack_off ~len:m.pack_len in
      let z = Bstr.create 0x1000 in
      let index (uid : Carton.Uid.t) =
        match idx with
        | None -> raise Not_found
        | Some idx ->
            let uid = Classeur.unsafe_uid_of_string (uid :> string) in
            Carton.Local (Classeur.find_offset idx uid) in
      Some (Carton.of_cache cache ~z ~allocate ~ref_length index)

  let reload blk =
    let header = header_of blk in
    let idx = idx_of blk in
    let carton = carton_of blk idx in
    { blk; header; idx; carton }

  let format ?ratio ?length blk =
    match Blk.format ?ratio ~rd ~wr ?length blk with
    | Ok _ -> Ok ()
    | Error `Invalid_metadata -> error_msgf "Invalid metadata"
    | Error (`Msg _) as err -> err

  let load blk =
    match Blk.make ~rd ~wr blk with
    | Ok blk -> Ok (reload blk)
    | Error (`Msg _) as err -> err

  let references t =
    match t.header with None -> [] | Some hdr -> Bundle.references hdr

  let prerequisites t =
    match t.header with None -> [] | Some hdr -> Bundle.prerequisites hdr

  let reference t name = List.assoc_opt name (references t)
  let is_empty t = references t = []

  let classeur_uid (uid : Carton.Uid.t) =
    Classeur.unsafe_uid_of_string (uid :> string)

  let exists t uid =
    match t.idx with
    | None -> false
    | Some idx -> Classeur.exists idx (classeur_uid uid)

  let uids t =
    match t.idx with
    | None -> []
    | Some idx ->
        let fn ~(uid : Classeur.uid) ~crc:_ ~offset:_ =
          Carton.Uid.unsafe_of_string (uid :> string) in
        Classeur.map ~fn idx

  let cursor t uid =
    match t.idx with
    | None -> None
    | Some idx ->
        begin match Classeur.find_offset idx (classeur_uid uid) with
        | cursor -> Some cursor
        | exception Not_found -> None
        end

  let kind t uid =
    match (t.carton, cursor t uid) with
    | Some carton, Some cursor -> Some (Carton.kind_of_offset carton ~cursor)
    | _ -> None

  let value t uid =
    match (t.carton, cursor t uid) with
    | Some carton, Some cursor ->
        let size = Carton.size_of_offset carton ~cursor Carton.Size.zero in
        let blob = Carton.Blob.make ~size in
        Some (Carton.of_offset carton blob ~cursor)
    | _ -> None

  let length t uid = Option.map Carton.Value.length (value t uid)

  let read t uid =
    let fn value =
      let len = Carton.Value.length value in
      let bstr = Carton.Value.bigstring value in
      (Carton.Value.kind value, Bstr.sub_string bstr ~off:0 ~len) in
    Option.map fn (value t uid)

  let publish t ?level ~references ~prerequisites ~load entries =
    let hdr = Bundle.make ~ref_length ~prerequisites references in
    match Bundle.check hdr with
    | Error (`Msg _) as err -> err
    | Ok () ->
        let hdr = Bundle.to_string hdr in
        let app = Blk.append t.blk `Inactive in
        let zone_off, _ = Blk.bounds t.blk `Inactive in
        let written = ref 0 in
        let push str =
          written := !written + String.length str;
          Blk.Append.append app str in
        let pad () =
          Blk.Append.flush app;
          written := 0;
          Blk.Append.position app - zone_off in
        begin
          try
            push hdr;
            let hdr_len = !written in
            let pack_off = pad () in
            let targets = Delta.delta ~load (List.to_seq entries) in
            let idx_entries, hash =
              Pack.emit ?level ~push ~load
                ~number_of_objects:(List.length entries) targets in
            let pack_len = !written in
            let idx_off = pad () in
            Pack.idx ~push ~pack:hash idx_entries;
            let idx_len = !written in
            let _ = pad () in
            let m = { hdr_len; pack_off; pack_len; idx_off; idx_len } in
            Log.debug (fun m' ->
                m' "publish: hdr:%d pack:[%d;%d] idx:[%d;%d]" m.hdr_len
                  m.pack_off m.pack_len m.idx_off m.idx_len);
            let blk = Blk.commit (Blk.with_metadata t.blk m) in
            Ok (reload blk)
          with Out_of_space -> Error `Zone_full
        end

  let to_bundle t =
    let m = Blk.metadata t.blk in
    if m.hdr_len = 0 then Seq.empty
    else
      Seq.append
        (Blk.seq t.blk `Active ~off:0 ~len:m.hdr_len ())
        (Blk.seq t.blk `Active ~off:m.pack_off ~len:m.pack_len ())

  module Tmp = struct
    type extern = Carton.Uid.t -> (Carton.Kind.t * Bstr.t) option

    let sink t = Blk.sink t.blk `Temporary
    let seq t ~len = Blk.seq t.blk `Temporary ~off:0 ~len ()

    let carton ?(extern = Fun.const None) t ~len =
      let cache = Blk.cachet t.blk `Temporary ~base:0 ~len in
      let z = Bstr.create 0x1000 in
      let index (uid : Carton.Uid.t) =
        match extern uid with
        | Some (kind, bstr) -> Carton.Extern (kind, bstr)
        | None -> raise Not_found in
      Carton.of_cache cache ~z ~allocate ~ref_length index
  end
end
