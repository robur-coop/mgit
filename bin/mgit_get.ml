module Git = Mgit_unix

let ( let* ) = Result.bind

let run _quiet hxd git format_of_output key =
  let* _perm, value = Git.get git key in
  match format_of_output with
  | Some `Hex -> Fmt.pr "@[<hov>%a@]%!" (Hxd_string.pp hxd) value; Ok ()
  | None | Some `Raw -> Fmt.pr "%s" value; Ok ()

open Cmdliner
open Mgit_cli

let format_of_output =
  let open Arg in
  let hex =
    let doc = "Displaying the object in the hexdump format." in
    info [ "hex" ] ~doc
  in
  let raw = info [ "raw" ] ~doc:"Displaying the object as is." in
  value & vflag None [ (Some `Hex, hex); (Some `Raw, raw) ]

let term =
  let open Term in
  const run $ setup_logs $ setup_hxd $ setup_git $ format_of_output $ Mgit_cli.entry
  |> map (Result.map_error (msgf "%a" Mgit.pp_error))
  |> term_result ~usage:false

let cmd =
  let doc = "Get the content of a file." in
  let man = [] in
  let info = Cmd.info ~doc ~man "get" in
  Cmd.v info term
