module Git = Mgit_unix

let ( let* ) = Result.bind

let short str = match String.split_on_char '/' str with
  | "refs" :: "heads" :: rem -> String.concat "/" rem
  | _ -> str

let run quiet git branch =
  Miou_unix.run @@ fun () ->
  let* git = Git.checkout git branch in
  match Git.commit git with
  | None -> if not quiet then Fmt.pr "%s\n%!" (short (Git.branch git)); Ok ()
  | Some (`Clean uid | `Dirty uid) ->
    if not quiet then Fmt.pr "%s %s\n%!" (short (Git.branch git)) (Mgit.uid_to_hex uid); Ok ()

open Cmdliner
open Mgit_cli

let new_branch =
  let doc = "The new branch of the Git repository to set." in
  let open Arg in
  required & pos 1 (some string) None & info [] ~doc ~docv:"BRANCH"

let term =
  let open Term in
  const run $ setup_logs $ setup_git $ new_branch
  |> map (Result.map_error (msgf "%a" Mgit.pp_error))
  |> term_result ~usage:false

let cmd =
  let doc = "Set the branch of the given Git repository." in
  let man = [] in
  let info = Cmd.info ~doc ~man "checkout" in
  Cmd.v info term
