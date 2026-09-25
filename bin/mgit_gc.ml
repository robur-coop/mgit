module Git = Mgit_unix

let run _quiet git shallows = Git.gc ?shallows git

open Cmdliner
open Mgit_cli

let depth =
  let parser str = match int_of_string_opt str with
    | Some n when n > 0 -> Ok n
    | Some _ -> error_msgf "The depth must be greater than 0"
    | None -> error_msgf "Invalid depth number" in
  Arg.conv (parser, Fmt.int)

let shallows =
  let doc = "The number of commits we would like to keep until to shallow." in
  let open Arg in
  value & pos 1 (some depth) None & info [] ~doc ~docv:"DEPTH"

let term =
  let open Term in
  const run $ setup_logs $ setup_git $ shallows
  |> map (Result.map_error (msgf "%a" Mgit.pp_error))
  |> term_result ~usage:false

let cmd =
  let doc = "Clean-up and shallow commits from a block-device (according to a depth)." in
  let man = [] in
  let info = Cmd.info ~doc ~man "gc" in
  Cmd.v info term
