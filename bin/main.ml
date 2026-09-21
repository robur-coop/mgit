open Cmdliner

let default =
  let open Term in
  ret (const (`Help (`Pager, None)))

let () =
  let doc = "A tool to manipulate a Git repository in OCaml." in
  let man = [] in
  let info = Cmd.info "mgit" ~doc ~man in
  let cmd = Cmd.group ~default info [ Mgit_create.cmd ] in
  Cmd.(exit (eval cmd))
