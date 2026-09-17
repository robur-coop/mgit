(* NOTE(dinosaure): mainly from mkernel-memtrace *)

exception Out_of_space

type t =
  { blk : Mkernel.Block.t
  ; sector : Bstr.t
  ; limit : int
  ; mutable cursor : int
  ; mutable fill : int
  ; mutable written : int }

let position t = t.cursor + t.fill
let written t = t.written
let full t = t.cursor + t.fill >= t.limit

let issue t =
  Mkernel.Block.atomic_write t.blk ~dst_off:t.cursor t.sector;
  t.cursor <- t.cursor + Bstr.length t.sector;
  t.fill <- 0

let rec fill_sectors t ~off ~len str =
  if len > 0 then begin
    let sector_size = Bstr.length t.sector in
    if t.cursor + t.fill + len > t.limit then raise Out_of_space;
    let can = Int.min len (sector_size - t.fill) in
    Bstr.blit_from_string str ~src_off:off t.sector ~dst_off:t.fill ~len:can;
    t.fill <- t.fill + can;
    t.written <- t.written + can;
    if t.fill = sector_size then issue t;
    fill_sectors t ~off:(off + can) ~len:(len - can) str
  end

let append t ?(off= 0) ?len str =
  let len = match len with
    | Some len -> len
    | None -> String.length str - off in
  if off < 0 || len < 0 || off > String.length str - len
  then invalid_arg "Append.append";
  fill_sectors t ~off ~len str

let flush t =
  if t.fill > 0 then begin
    let sector_size = Bstr.length t.sector in
    Bstr.fill t.sector ~off:t.fill ~len:(sector_size - t.fill) '\000';
    issue t
  end

let create blk ?(off= 0) limit =
  let sector_size = Mkernel.Block.sector_size blk in
  if off land (sector_size - 1) <> 0
  then invalid_arg "Append.make: off must be sector-aligned";
  let sector = Bstr.create sector_size in
  { blk; sector; limit; cursor= off; fill= 0; written= 0 }

let sink ~init =
  let push t str = append t str; t in
  Flux.Sink { init; push; full; stop= flush }

let device ?off ~name limit =
  let fn blk () = create blk ?off limit in
  Mkernel.map fn [ Mkernel.block name ]
  |> Mkernel.finally flush
  
