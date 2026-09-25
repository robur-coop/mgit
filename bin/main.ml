open Cmdliner

let default =
  let open Term in
  ret (const (`Help (`Pager, None)))

let () =
  let doc = "A tool to manipulate a Git repository in OCaml." in
  let man = [] in
  let info = Cmd.info "mgit" ~doc ~man ~envs:[ Mgit_cli.ssh; Mgit_cli.date ] in
  let cmd = Cmd.group ~default info
    [ Mgit_create.cmd; Mgit_set.cmd; Mgit_get.cmd; Mgit_head.cmd; Mgit_list.cmd
    ; Mgit_bundle.cmd; Mgit_gc.cmd ] in
  Cmd.(exit (eval cmd))
