module Git = Mgit_unix

let ( let* ) = Result.bind
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let run _quiet git =
  match Git.commit git with
  | Some (`Clean uid) -> Fmt.pr "%s\n%!" (Mgit.uid_to_hex uid); Ok ()
  | Some (`Dirty uid) -> Fmt.pr "~%s\n%!" (Mgit.uid_to_hex uid); Ok ()
  | None -> error_msgf "No commit from the given block-device"

open Cmdliner
open Mgit_cli

let term =
  let open Term in
  const run $ setup_logs $ setup_git 
  |> term_result ~usage:false

let cmd =
  let doc = "Show the last commit (HEAD)." in
  let man = [] in
  let info = Cmd.info ~doc ~man "head" in
  Cmd.v info term
