module Git = Mgit_unix

let ( let* ) = Result.bind

let run quiet git =
  Miou_unix.run @@ fun () ->
  let* changes = Git.pull git in
  let fn0 branch = function
    | `Add path -> Fmt.pr "%s: %a %s\n%!" branch Fmt.(styled (`Fg `Green) string) "+" path
    | `Rem path -> Fmt.pr "%s: %a %s\n%!" branch Fmt.(styled (`Fg `Red) string) "-" path
    | `Set path -> Fmt.pr "%s: %a %s\n%!" branch Fmt.(styled (`Fg `Yellow) string) "~" path in
  let fn1 (branch, change) = List.iter (fn0 branch) change in
  if not quiet then List.iter fn1 changes; Ok ()

open Cmdliner
open Mgit_cli

let term =
  let open Term in
  const run $ setup_logs $ setup_git
  |> map (Result.map_error (msgf "%a" Mgit.pp_error))
  |> term_result ~usage:false

let cmd =
  let doc = "Pull a Git repository and fill the given block device with new Git objects." in
  let man = [] in
  let info = Cmd.info ~doc ~man "pull" in
  Cmd.v info term
