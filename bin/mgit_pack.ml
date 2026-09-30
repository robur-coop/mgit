module Git = Mgit_unix

let run _quiet git haves wants output =
  let oc, finally = match output with
    | Some filename ->
      let oc = open_out_bin (Fpath.to_string filename) in
      let finally () = close_out oc in
      (oc, finally)
    | None ->
      Stdlib.set_binary_mode_out stdout true;
      (stdout, ignore) in
  Fun.protect ~finally @@ fun () ->
  Git.to_pack git ~haves wants (output_string oc)

open Cmdliner
open Mgit_cli

let output =
  let doc = "The PACK file." in
  let open Arg in
  value & opt (some non_existing_file) None & info [ "o"; "output" ] ~doc ~docv:"FILE"

let haves =
  let doc = "Commits that we have." in
  let open Arg in
  value & opt (list uid) [] & info [ "have" ] ~doc ~docv:"UID"

let wants =
  let doc = "Commits that the block-device have and we want." in
  let open Arg in
  value & opt (list uid) [] & info [ "want" ] ~doc ~docv:"UID"

let term =
  let open Term in
  const run $ setup_logs $ setup_git $ haves $ wants $ output
  |> map (Result.map_error (msgf "%a" Mgit.pp_error))
  |> term_result ~usage:false

let cmd =
  let doc = "Generate a PACK file from a block-device." in
  let man = [] in
  let info = Cmd.info ~doc ~man "pack" in
  Cmd.v info term
