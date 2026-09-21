module Block = Mgit_unix.Block
module Git = Mgit_unix

let ( let* ) = Result.bind
let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt

let run _quiet (size, _) filename =
  Miou_unix.run ~domains:0 @@ fun () ->
  Mirage_crypto_rng_unix.use_default ();
  let* blk = Block.create filename size in
  Git.format blk

open Cmdliner
open Mgit_cli

let size =
  let doc = "The size of the block device." in
  let open Arg in
  value & opt size (512 * 8192) (* 4M *) & info [ "s"; "size" ] ~doc ~docv:"SIZE"

let is_power_of_two x = x <> 0 && x land (lnot x + 1) = x

let sector_size =
  let parser str = match int_of_string_opt str with
    | Some n when is_power_of_two n && n >= 512 -> Ok n
    | Some _ -> error_msgf "The given sector-size is not a power of two"
    | None -> error_msgf "Invalid sector-size" in
  Arg.conv (parser, Fmt.int)

let sector_size =
  let doc = "The sector-size of the block device." in
  let open Arg in
  value & opt sector_size 512 & info [ "sector-size" ] ~doc ~docv:"SIZE"

let setup_size size sector_size =
  if size land (sector_size - 1) <> 0
  then error_msgf "The given size is not sector-size aligned"
  else Ok (size, sector_size)

let setup_size =
  let open Term in
  const setup_size $ size $ sector_size
  |> term_result ~usage:true

let filename =
  let doc = "The block device." in
  let open Arg in
  required & pos 0 (some non_existing_file) None & info [] ~doc ~docv:"FILENAME"

let term =
  let open Term in
  const run $ setup_logs $ setup_size $ filename
  |> map (Result.map_error (msgf "%a" Mgit.pp_error))
  |> term_result ~usage:false

let cmd =
  let doc = "Create a new block device and format it." in 
  let man = [] in
  let info = Cmd.info ~doc ~man "create" in
  Cmd.v info term
