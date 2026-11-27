open Cmdliner

let default =
  let open Term in
  ret (const (`Help (`Pager, None)))

let () =
  let doc =
    "A tool to manipulate a Git repository as a block device in OCaml."
  in
  let man =
    [
      `P
        "Typically, a Git repository consists of the repository's contents, \
         along with a $(i,.git) subfolder that the Git tool can manipulate. \
         With mgit, the aim is to create an image file containing all the \
         information relating to your Git repository and to be able to extract \
         its current contents from it. It is therefore no longer a matter of \
         working with a folder containing a $(i,.git) subfolder, but rather of \
         working with a single file containing everything necessary and \
         representing your Git repository."
    ; `P
        "The advantage of this approach is that it allows us to reuse this \
         method of representing the Git repository as a $(i,block-device) - \
         that is, a persistent memory area whose contents can be accessed (and \
         modified) page by page, just like a hard disk."
    ; `P
        "This allows us to mount such a file as an image on a unikernel so \
         that the latter can manipulate your Git repository without needing to \
         know what a file system is (which is necessary to understand the \
         contents of your Git repository and the $(i,.git) subfolder)."
    ; `P
        "The $(tname) tool is therefore a tool for manipulating this file \
         (which is your Git repository) in order to add, modify or delete \
         files. You can also create, modify or delete branches and synchronise \
         with other Git repositories."; `S "USAGE"
    ; `P
        "Here is a simple example of using $(tname) to show how you can modify \
         a remote repository without going through the whole process (git \
         clone, git add, git commit) on a folder, but directly from your \
         $(i,block-device). We assume that your SSH client is correctly \
         configured to authenticate you on the chosen repository."
    ; `Pre
        "\\$ mgit create --size=10MiB disk.img\n\
         \\$ mgit pull disk.img -r git@remote:user/repo.git\n\
         \\$ echo 'Hello World!' | mgit set disk.img -r \
         git@remote:user/repo.git /mgit.txt -"
    ; `P
        "In this example, we create a file called $(b,disk.img) which will \
         contain our repository $(b,git@remote:user/repo.git) (the size may \
         depend on your repository). Next, we synchronise our \
         $(i,block-device) with our repository. Finally, we add a file called \
         $(b,mgit.txt) to the repository, and it will be automatically pushed \
         to your remote repository."
    ]
  in
  let info = Cmd.info "mgit" ~doc ~man ~envs:[ Mgit_cli.ssh; Mgit_cli.date ] in
  let cmd =
    Cmd.group ~default info
      [
        Mgit_create.cmd; Mgit_set.cmd; Mgit_get.cmd; Mgit_head.cmd
      ; Mgit_list.cmd; Mgit_bundle.cmd; Mgit_gc.cmd; Mgit_repl.cmd
      ; Mgit_pull.cmd; Mgit_branches.cmd; Mgit_pack.cmd; Mgit_checkout.cmd
      ]
  in
  Cmd.(exit (eval cmd))
