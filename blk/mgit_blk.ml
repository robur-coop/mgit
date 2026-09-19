let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let ( let* ) = Result.bind
let open_error = function Ok _ as v -> v | Error `Invalid_metadata as err -> err

let src = Logs.Src.create "mgit.blk"

module Log = (val Logs.src_log src : Logs.LOG)
module Device = Device
module Append = Append

module type BLOCK = Device.S

type zone =
  [ `Active
  | `Inactive
  | `Temporary ]

let pp_error ppf = function
  | `Invalid_metadata -> Fmt.string ppf "Invalid metadata"
  | `Msg msg -> Fmt.string ppf msg

let magic = "deadbeef"
let switch = function `A -> `B | `B -> `A

let pp_zone ppf = function
  | `T -> Fmt.string ppf "tmp"
  | `A -> Fmt.string ppf "zone A"
  | `B -> Fmt.string ppf "zone B"

module Make (Block : BLOCK) = struct
  module Append = Append.Make (Block)

  type 'metadata t =
    { generation : int64
    ; active : [ `A | `B ]
    ; buf_off : int
    ; buf_len : int
    ; zone_a_off : int
    ; zone_a_len : int
    ; zone_b_off : int
    ; zone_b_len : int
    ; wr : 'metadata -> Bstr.t -> int
    ; rd : Bstr.t -> ('metadata, [ `Invalid_metadata ]) result
    ; metadata : 'metadata
    ; blk : Block.t }

  let metadata { metadata; _ } = metadata
  let with_metadata t metadata = { t with metadata }

  let zone t = function
    | `Temporary -> `T
    | `Active -> (t.active :> [ `A | `B | `T ])
    | `Inactive -> (switch t.active :> [ `A | `B | `T ])

  let bounds t which = match zone t which with
    | `T -> (t.buf_off, t.buf_len)
    | `A -> (t.zone_a_off, t.zone_a_len)
    | `B -> (t.zone_b_off, t.zone_b_len)

  let mapper which =
    fun t ~pos:rel_off len ->
    let sector_size = Block.sector_size t.blk in
    if len <> sector_size
    then Fmt.invalid_arg "Blk.mapper: [len] (%d, sector:%d) is not sector-aligned" len sector_size;
    let off, max = bounds t which in
    let abs_off = off + rel_off in
    if abs_off < 0 || rel_off < 0 || len < 0 || rel_off > max - len
    then Fmt.invalid_arg "Blk.mapper: out of bounds ([%d:%d], slot %d, %a)" abs_off len
           (Int64.to_int t.generation land 1) pp_zone (zone t which);
    let bstr = Bstr.create len in
    Block.atomic_read t.blk ~src_off:abs_off bstr;
    bstr

  (* NOTE(dinosaure): unlike {!val:mapper}, a [Cachet.map] must never fail: an
     empty bigstring is returned when the requested page falls outside of
     [\[base; base + len\[]. This is the contract expected by [Cachet]. *)
  let reader which ~base ~len =
    fun t ~pos:rel_off req ->
    let sector_size = Block.sector_size t.blk in
    let off, max = bounds t which in
    let base_max = Int.min len (max - base) in
    if rel_off < 0 || rel_off >= base_max || req <> sector_size
    then Bstr.empty
    else begin
      let abs_off = off + base + rel_off in
      let bstr = Bstr.create sector_size in
      Block.atomic_read t.blk ~src_off:abs_off bstr;
      let available = base_max - rel_off in
      if available >= sector_size
      then bstr else Bstr.sub bstr ~off:0 ~len:available
    end

  let cachet t which ~base ~len =
    let sector_size = Block.sector_size t.blk in
    if base land (sector_size - 1) <> 0
    then Fmt.invalid_arg "Blk.cachet: [base] (%d) is not sector-aligned" base;
    Cachet.make ~pagesize:sector_size ~map:(reader which ~base ~len) t

  let writev which =
    fun t ~pos:rel_off bstrs ->
    let sector_size = Block.sector_size t.blk in
    let off, max = bounds t which in
    let len = List.fold_left (fun acc bstr -> acc + Bstr.length bstr) 0 bstrs in
    if rel_off < 0 || rel_off land (sector_size - 1) <> 0
    then Fmt.invalid_arg "Blk.writer: [pos] (%d, sector:%d) is not sector-aligned" rel_off sector_size;
    if len > max - rel_off
    then Fmt.invalid_arg "Blk.writer: out of bounds ([%d:%d], %a)" (off + rel_off) len
           pp_zone (zone t which);
    let write dst_off bstr =
      if Bstr.length bstr <> sector_size
      then Fmt.invalid_arg "Blk.writer: page (%d, sector:%d) is not sector-aligned"
             (Bstr.length bstr) sector_size;
      Block.atomic_write t.blk ~dst_off bstr;
      dst_off + sector_size in
    ignore (List.fold_left write (off + rel_off) bstrs)

  let writer t which =
    let sector_size = Block.sector_size t.blk in
    let _, len = bounds t which in
    let number_of_pages = len / sector_size in
    Cachet_wr.make ~pagesize:sector_size ~map:(mapper which) ~writev:(writev which)
      ~number_of_pages t

  let append t which =
    let off, len = bounds t which in
    Append.create t.blk ~off (off + len)

  let seq t which ?(off= 0) ?len () =
    let sector_size = Block.sector_size t.blk in
    let zone_off, zone_len = bounds t which in
    let len = match len with
      | Some len -> Int.min len (zone_len - off)
      | None -> zone_len - off in
    let bstr = Bstr.create sector_size in
    let rec go pos () =
      if pos >= len then Seq.Nil
      else begin
        (* NOTE(dinosaure): the block-device can only be read sector by sector,
           so we align the read down and slice afterwards. *)
        let abs = zone_off + off + pos in
        let aligned = abs land lnot (sector_size - 1) in
        let skip = abs - aligned in
        Block.atomic_read t.blk ~src_off:aligned bstr;
        let n = Int.min (sector_size - skip) (len - pos) in
        Seq.Cons (Bstr.sub_string bstr ~off:skip ~len:n, go (pos + n))
      end in
    go 0

  let source t which ?off ?len () =
    Flux.Source.seq (seq t which ?off ?len ())

  let sink t which = Append.sink ~init:(fun () -> append t which)

  let crc bstr = Checkseum.Crc32.(digest_bigstring bstr 0 (Bstr.length bstr - 4) default) |> Optint.to_int32
  let int64 = Int64.of_int

  let store t bstr =
    let sector_size = Bstr.length bstr in
    Bstr.fill bstr '\000';
    Bstr.blit_from_string magic ~src_off:0 bstr ~dst_off:0 ~len:8;
    Bstr.set_int64_le bstr 8 t.generation;
    Bstr.set_uint8 bstr 16 (match t.active with `A -> 0 | `B -> 1);
    Bstr.set_int64_le bstr 24 (int64 t.buf_off);
    Bstr.set_int64_le bstr 32 (int64 t.buf_len);
    Bstr.set_int64_le bstr 40 (int64 t.zone_a_off);
    Bstr.set_int64_le bstr 48 (int64 t.zone_a_len);
    Bstr.set_int64_le bstr 56 (int64 t.zone_b_off);
    Bstr.set_int64_le bstr 64 (int64 t.zone_b_len);
    let payload = Bstr.sub bstr ~off:72 ~len:(sector_size - 4 - 72) in
    let _len = t.wr t.metadata payload in
    let crc = crc bstr in
    Bstr.set_int32_le bstr (sector_size - 4) crc

  let write blk t =
    let sector_size = Block.sector_size blk in
    let bstr = Bstr.create sector_size in
    store t bstr;
    let slot = Int64.to_int t.generation land 1 in
    Block.atomic_write blk ~dst_off:(slot * sector_size) bstr

  let sync t =
    let t = { t with generation= Int64.add t.generation 1L } in
    write t.blk t; t

  let commit t =
    let active = switch t.active in
    let t = { t with generation= Int64.add t.generation 1L; active } in
    write t.blk t; t

  let load blk ~wr ~rd bstr =
    let sector_size = Bstr.length bstr in
    if Bstr.sub_string bstr ~off:0 ~len:8 <> magic
    then error_msgf "Invalid superblock magic"
    else if Bstr.get_int32_le bstr (sector_size - 4) <> crc bstr
    then error_msgf "Corrupted superblock"
    else begin
      let generation = Bstr.get_int64_le bstr 8 in
      let active = match Bstr.get_uint8 bstr 16 with 0 -> `A | _ -> `B in
      let* metadata = rd (Bstr.sub bstr ~off:72 ~len:(sector_size - 4 - 72)) |> open_error in
      let buf_off = Bstr.get_int64_le bstr 24 |> Int64.to_int in
      let buf_len = Bstr.get_int64_le bstr 32 |> Int64.to_int in
      let zone_a_off = Bstr.get_int64_le bstr 40 |> Int64.to_int in
      let zone_a_len = Bstr.get_int64_le bstr 48 |> Int64.to_int in
      let zone_b_off = Bstr.get_int64_le bstr 56 |> Int64.to_int in
      let zone_b_len = Bstr.get_int64_le bstr 64 |> Int64.to_int in
      Ok { generation; active
         ; buf_off; buf_len
         ; zone_a_off; zone_a_len
         ; zone_b_off; zone_b_len
         ; wr; rd
         ; metadata
         ; blk }
    end

  let make ~wr ~rd blk =
    let sector_size = Block.sector_size blk in
    let bstr = Bstr.create sector_size in
    let slot n =
      Block.atomic_read blk ~src_off:(n * sector_size) bstr;
      match load blk ~wr ~rd bstr with
      | Ok t -> Some t
      | Error err ->
        Log.warn (fun m -> m "Superblock (slot %d): %a" n pp_error err);
        None in
    match slot 0, slot 1 with
    | Some a, Some b ->
      if Int64.compare a.generation b.generation >= 0
      then Ok a else Ok b
    | Some x, None | None, Some x -> Ok x
    | None, None -> error_msgf "No valid superblock found"

  let format ?(ratio= 1. /. 3.) ~rd ~wr ?length blk =
    let sector_size = Block.sector_size blk in
    let length = match length with
      | Some length when length land (sector_size - 1) = 0 -> length
      | Some length -> Fmt.invalid_arg "The given length (%d) is not sector-aligned (%d)" length sector_size
      | None -> Block.length blk in
    if ratio <= 0. || ratio >= 1. then invalid_arg "Blk.format: bad ratio";
    let usable = length - (2 * sector_size) in
    if usable < 6 * sector_size then error_msgf "Block-device too small"
    else begin
      let align_down v = v land lnot (sector_size - 1) in
      let buf_off = 2 * sector_size in
      let buf_len = align_down (int_of_float (float_of_int usable *. ratio)) in
      let zone_len = align_down ((usable - buf_len) / 2) in
      let zone_a_off = buf_off + buf_len in
      let zone_b_off = zone_a_off + zone_len in
      let empty = Bstr.make (sector_size - 4 - 72) '\000' in
      let* metadata = rd empty |> open_error in
      let t = { generation= 0L
              ; active= `A
              ; buf_off; buf_len
              ; zone_a_off; zone_a_len= zone_len
              ; zone_b_off; zone_b_len= zone_len
              ; wr; rd
              ; metadata
              ; blk } in
      write blk t; Ok t
    end
end
