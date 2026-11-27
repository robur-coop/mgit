module Git = Mgit_unix

let ( let* ) = Result.bind

let run quiet git =
  Miou_unix.run @@ fun () ->
  let* changes = Git.pull git in
  let fn0 branch = function
    | `Add path ->
        Fmt.pr "%s: %a %s\n%!" branch Fmt.(styled (`Fg `Green) string) "+" path
    | `Rem path ->
        Fmt.pr "%s: %a %s\n%!" branch Fmt.(styled (`Fg `Red) string) "-" path
    | `Set path ->
        Fmt.pr "%s: %a %s\n%!" branch Fmt.(styled (`Fg `Yellow) string) "~" path
  in
  let fn1 (branch, change) = List.iter (fn0 branch) change in
  if not quiet then List.iter fn1 changes;
  Ok ()

open Cmdliner
open Mgit_cli

let term =
  let open Term in
  const run
  $ setup_logs
  $ setup_git
  |> map (Result.map_error (msgf "%a" Mgit.pp_error))
  |> term_result ~usage:false

let cmd =
  let doc =
    "Pull a Git repository and fill the given block device with new Git \
     objects."
  in
  let man =
    [
      `S "SYNOPSIS"
    ; `P
        "It is possible for a $(i,block-device) to synchronise with a remote \
         Git repository on a specific branch. In this case, $(tname) supports \
         several protocols, such as SSH and HTTPS, or synchronisation with a \
         local repository."
    ; `P
        "In the case of SSH, $(tname) uses your $(b,ssh) client directly, but \
         you can modify the command to use via $(b,MGIT_SSH) (if you wish to \
         add more options or use a different program)."
    ; `P
        "As for HTTP, $(tname) uses its own OCaml implementation of HTTP as \
         well as its own OCaml implementation of TLS."
    ; `P
        "During synchronisation, $(tname) displays the changes that have taken \
         place in the repository between the latest commit on the remote \
         repository and the current commit, provided that the negotiation was \
         successful."; `S "MEMORY"
    ; `P
        "Synchronisation does not involve any temporary files on your hard \
         drive, and all the information required for this process is handled \
         on the $(i,block-device). This means that $(tname) may fail because \
         there is not enough space on the given $(i,block-device): you will \
         therefore need to expand it."
    ]
  in
  let envs = [ Mgit_cli.ssh ] in
  let info = Cmd.info ~doc ~man ~envs "pull" in
  Cmd.v info term
