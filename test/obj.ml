module Object = Mgit.Object

let exitf fmt = Fmt.kstr (fun msg -> Fmt.epr "%s: %s\n%!" Sys.executable_name msg; exit 1) fmt
let or_exit = function Ok v -> v | Error (`Msg msg) -> exitf "%s" msg

let stdin () =
  let buf = Buffer.create 0x7ff in
  let tmp = Bytes.create 0x7ff in
  let rec go () = match input stdin tmp 0 (Bytes.length tmp) with
    | 0 -> Buffer.contents buf
    | len -> Buffer.add_subbytes buf tmp 0 len; go ()
    | exception End_of_file -> Buffer.contents buf in
  Stdlib.set_binary_mode_in stdin true; go ()

let lines str =
  let fn = ( <> ) "" in
  List.filter fn (String.split_on_char '\n' str)

let entry_of_ls_tree line =
  match String.index_opt line '\t' with
  | None -> exitf "Invalid ls-tree line: %S" line
  | Some tab ->
    let name = String.sub line (tab+1) (String.length line - tab - 1) in
    let meta = String.sub line 0 tab in
    let fn = ( <> ) "" in
    let words = List.filter fn (String.split_on_char ' ' meta) in
    match words with
    | [ mode; _k; hex ] ->
      let node = or_exit (Object.uid_of_hex hex) in
      let perm = match mode with
        | "100644" -> `Normal
        | "100755" -> `Exec
        | "100664" -> `Everybody
        | "120000" -> `Link
        | "040000" | "40000" -> `Dir
        | "160000" -> `Commit
        | mode -> exitf "Invalid mode: %S" mode in
      { Object.Tree.perm; name; node }
    | _ -> exitf "Invalid ls-tree line: %S" line

let string_of_perm = function
  | `Normal -> "100644"
  | `Exec -> "100755"
  | `Everybody -> "100664"
  | `Link -> "120000"
  | `Dir -> "040000"
  | `Commit -> "160000"

let kind_of_perm = function
  | `Dir -> "tree"
  | `Commit -> "commit"
  | _ -> "blob"

let user which =
  let get suffix ~default = Option.value ~default (Sys.getenv_opt (Fmt.str "GIT_%s_%s" which suffix)) in
  let name = get "NAME" ~default:"mgit" in
  let email = get "EMAIL" ~default:"mgit@uniker.nl" in
  let date = get "DATE" ~default:"0 +0000" in
  let date = Fmt.str "x <x> %s" date in
  let { Object.User.date; _ } = or_exit (Object.User.of_string date) in
  { Object.User.name; email; date }

let hash_object k =
  let k = or_exit (Object.kind_of_string k) in
  let payload = stdin () in
  Fmt.pr "%s\n%!" (Object.hex_of_uid (Object.digest ~kind:k payload))

let mktree () =
  let entries = List.map entry_of_ls_tree (lines (stdin ())) in
  let tree = Object.Tree.v entries in
  Fmt.pr "%s\n%!" (Object.hex_of_uid (Object.Tree.digest tree))

let cat_tree () =
  let tree = or_exit (Object.Tree.of_string (stdin ())) in
  let fn { Object.Tree.perm; name; node } =
    Fmt.pr "%s %s %s\t%s\n" (string_of_perm perm)
      (kind_of_perm perm) (Object.hex_of_uid node) name in
  List.iter fn (Object.Tree.to_list tree)

let commit_tree tree parents message =
  let tree = or_exit (Object.uid_of_hex tree) in
  let parents = List.map (fun hex -> or_exit (Object.uid_of_hex hex)) parents in
  let author = user "AUTHOR"
  and committer = user "COMMITTER" in
  let commit = Object.Commit.make ~tree ~parents ~author ~committer
    (Some message) in
  Fmt.pr "%s\n%!" (Object.hex_of_uid (Object.Commit.digest commit))

let cat_commit () =
  let commit = or_exit (Object.Commit.of_string (stdin ())) in
  Fmt.pr "tree %s\n%!" (Object.hex_of_uid commit.Object.Commit.tree);
  let fn uid = Fmt.pr "parent %s\n%!" (Object.hex_of_uid uid) in
  List.iter fn commit.Object.Commit.parents;
  Fmt.pr "author %s\n%!" (Object.User.to_string commit.Object.Commit.author);
  Fmt.pr "committer %s\n%!" (Object.User.to_string commit.Object.Commit.committer);
  Option.iter (Fmt.pr "\n%s%!") commit.Object.Commit.message

let iso k =
  let payload = stdin () in
  let str = match or_exit (Object.kind_of_string k) with
    | `A -> Object.Commit.to_string (or_exit (Object.Commit.of_string payload))
    | `B -> Object.Tree.to_string (or_exit (Object.Tree.of_string payload))
    | `C -> payload
    | `D -> Object.Tag.to_string (or_exit (Object.Tag.of_string payload)) in
  if String.equal payload str
  then Fmt.pr "iso ok\n%!"
  else exitf "iso failed:\n%S\nversus\n%S" payload str

let usage () =
  Fmt.epr "usage: obj <command>\n\n\
commands:\n\
\ hash-object <kind> (payload on stdin)
\ mktree             git-ls-tree output on stdin\n\
\ cat-tree           raw tree object on stdin\n\
\ commit-tree <tree> [-p <uid>] -m <msg>\n\
\ cat-commit         raw commit object on stdin\n
\ iso <kind>         raw object on stdin\n\
%!";
  exit 1

let () = match Array.to_list Sys.argv with
  | _ :: "hash-object" :: k :: _ -> hash_object k
  | _ :: "mktree" :: _ -> mktree ()
  | _ :: "cat-tree" :: _ -> cat_tree ()
  | _ :: "cat-commit" :: _ -> cat_commit ()
  | _ :: "iso" :: k :: _ -> iso k
  | _ :: "commit-tree" :: tree :: rem ->
    let rec go parents message = function
      | "-p" :: uid :: rem -> go (uid :: parents) message rem
      | "-m" :: msg :: rem -> go parents msg rem
      | [] -> commit_tree tree (List.rev parents) message
      | arg :: _ -> exitf "Unknown argument: %S" arg in
    go [] "" rem
  | _ -> usage ()
