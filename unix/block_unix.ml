let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

type t = { fd : Unix.file_descr; sector_size : int; length : int }

let sector_size { sector_size; _ } = sector_size
let length { length; _ } = length

let pread fd buf ~off =
  let _ = Unix.lseek fd off Unix.SEEK_SET in
  let len = Bytes.length buf in
  let rec go dst_off =
    if dst_off < len then
      match Unix.read fd buf dst_off (len - dst_off) with
      | 0 -> Bytes.fill buf dst_off (len - dst_off) '\000'
      | n -> go (dst_off + n) in
  go 0

let pwrite fd buf ~off =
  let _ = Unix.lseek fd off Unix.SEEK_SET in
  let len = Bytes.length buf in
  let rec go src_off =
    if src_off < len then
      let n = Unix.write fd buf src_off (len - src_off) in
      go (src_off + n) in
  go 0

let atomic_read t ~src_off ?(dst_off = 0) bstr =
  if src_off land (t.sector_size - 1) <> 0
  then Fmt.invalid_arg "Block_unix.atomic_read: unaligned offset (%d)" src_off;
  let buf = Bytes.create t.sector_size in
  pread t.fd buf ~off:src_off;
  Bstr.blit_from_bytes buf ~src_off:0 bstr ~dst_off ~len:t.sector_size

let atomic_write t ?(src_off = 0) ~dst_off bstr =
  if dst_off land (t.sector_size - 1) <> 0
  then Fmt.invalid_arg "Block_unix.atomic_write: unaligned offset (%d)" dst_off;
  let buf = Bytes.create t.sector_size in
  Bstr.blit_to_bytes bstr ~src_off buf ~dst_off:0 ~len:t.sector_size;
  pwrite t.fd buf ~off:dst_off

let of_fd ~sector_size fd =
  let stat = Unix.fstat fd in
  let length = stat.Unix.st_size land lnot (sector_size - 1) in
  { fd; sector_size; length }

let check_sector_size sector_size =
  if sector_size <= 0 || sector_size land (sector_size - 1) <> 0
  then error_msgf "Invalid sector size: %d (must be a power of two)" sector_size
  else Ok ()

let load ?(sector_size = 0x1000) filename =
  match check_sector_size sector_size with
  | Error _ as err -> err
  | Ok () ->
      begin match Unix.openfile filename Unix.[ O_RDWR ] 0o644 with
      | fd -> Ok (of_fd ~sector_size fd)
      | exception Unix.Unix_error (err, _, _) ->
          error_msgf "%s: %s" filename (Unix.error_message err)
      end

let create ?(sector_size = 0x1000) filename length =
  match check_sector_size sector_size with
  | Error _ as err -> err
  | Ok () when length land (sector_size - 1) <> 0 ->
      error_msgf "%s: length (%d) is not sector-aligned (%d)" filename length
        sector_size
  | Ok () ->
      let m = Unix.[ O_RDWR; O_CREAT; O_TRUNC ] in
      begin match Unix.openfile filename m 0o644 with
      | fd -> Unix.ftruncate fd length; Ok (of_fd ~sector_size fd)
      | exception Unix.Unix_error (err, _, _) ->
          error_msgf "%s: %s" filename (Unix.error_message err)
      end

let close t = Unix.close t.fd
