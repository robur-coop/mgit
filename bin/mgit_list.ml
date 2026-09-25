module Git = Mgit_unix

let ( let* ) = Result.bind

let run _quiet git path =
  let* lst = Git.list git path in
  let fn (path, k) = match k with
    | `Value -> Fmt.pr "%s\n%!" path
    | `Dictionary -> Fmt.pr "%s/\n%!" path in
  List.iter fn lst;
  Ok ()

open Cmdliner
open Mgit_cli

let term =
  let open Term in
  const run $ setup_logs $ setup_git $ Mgit_cli.path
  |> map (Result.map_error (msgf "%a" Mgit.pp_error))
  |> term_result ~usage:false

let cmd =
  let doc = "List entries from a given block-device and a path." in
  let man = [] in
  let info = Cmd.info ~doc ~man "list" in
  Cmd.v info term
