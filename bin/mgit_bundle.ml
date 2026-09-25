module Git = Mgit_unix

let run _quiet git output =
  let oc, finally = match output with
    | Some filename ->
      let oc = open_out_bin (Fpath.to_string filename) in
      let finally () = close_out oc in
      (oc, finally)
    | None -> (stdout, ignore) in
  Fun.protect ~finally @@ fun () ->
  Git.to_bundle git (output_string oc);
  Ok ()

open Cmdliner
open Mgit_cli

let output =
  let doc = "The output file." in
  let open Arg in
  value & opt (some non_existing_file) None & info [ "o"; "output" ] ~doc ~docv:"FILENAME"

let term =
  let open Term in
  const run $ setup_logs $ setup_git $ output
  |> term_result ~usage:false

let cmd =
  let doc = "Generate a git bundle file from a block-device." in
  let man = [] in
  let info = Cmd.info ~doc ~man "bundle" in
  Cmd.v info term
