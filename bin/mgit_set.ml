module Git = Mgit_unix

let ( let* ) = Result.bind

let contents_of_in_channel ic =
  let tmp = Bytes.create 0x7ff in
  let buf = Buffer.create 0x7ff in
  let rec go () = match input ic tmp 0 (Bytes.length tmp) with
    | 0 | exception End_of_file -> Buffer.contents buf
    | len -> Buffer.add_subbytes buf tmp 0 len; go () in
  go ()

let run _quiet git message path input =
  let contents = match input with
    | Some None -> contents_of_in_channel stdin
    | Some (Some filename) ->
      let ic = open_in_bin (Fpath.to_string filename) in
      let finally () = close_in ic in
      Fun.protect ~finally @@ fun () ->
      contents_of_in_channel ic
    | None -> String.empty in
  let result = Git.change_and_push git ?message @@ fun git ->
    Git.set git path contents in
  let* () = Result.join result in
  let uid = Git.commit git |> Option.get in
  match uid with
  | `Clean uid -> Fmt.pr "%s\n%!" (Mgit.uid_to_hex uid); Ok ()
  | `Dirty uid -> Fmt.pr "~%s\n%!" (Mgit.uid_to_hex uid); Ok ()

open Cmdliner
open Mgit_cli

let message =
  let doc = "The message of the commit." in
  let open Arg in
  value & opt (some string) None & info [ "m" ] ~doc ~docv:"STRING"

let contents =
  let doc = "The contents of the entry." in
  let parser str = match str, Fpath.of_string str with
    | "-", _ -> Ok None
    | _, Ok v when Sys.file_exists str && Sys.is_regular_file str -> Ok (Some v)
    | _, Ok v -> error_msgf "%a is not a file and/or does not exists" Fpath.pp v
    | _, (Error _ as err) -> err in
  let pp ppf = function
    | None -> Fmt.string ppf "-"
    | Some v -> Fpath.pp ppf v in
  let contents = Arg.conv (parser, pp) in
  let open Arg in
  value & pos 2 (some contents) None & info [] ~doc ~docv:"CONTENTS"

let term =
  let open Term in
  const run $ setup_logs $ setup_git $ message $ Mgit_cli.entry $ contents
  |> map (Result.map_error (msgf "%a" Mgit.pp_error))
  |> term_result ~usage:false

let cmd =
  let doc = "Populate a block device which represents a Git repository with a new entry." in
  let man = [] in
  let info = Cmd.info ~doc ~man "set" in
  Cmd.v info term
