module Git = Mgit_unix

let ( let* ) = Result.bind

let run quiet git =
  let fn (branch, uid) = Fmt.pr "%s %s\n%!" branch (Mgit.uid_to_hex uid) in
  let branches = Git.branches git in
  if not quiet then List.iter fn branches; Ok ()

open Cmdliner
open Mgit_cli

let term =
  let open Term in
  const run $ setup_logs $ setup_git
  |> map (Result.map_error (msgf "%a" Mgit.pp_error))
  |> term_result ~usage:false

let cmd =
  let doc = "Show availabe branches from the given block device." in
  let man = [] in
  let info = Cmd.info ~doc ~man "branches" in
  Cmd.v info term
