module Flow = Mgit_unix.Flow
module Git = Mgit_unix

let ( let* ) = Result.bind
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt
let now = Mgit_cli.now

let print_changes changes =
  let short str = match String.split_on_char '/' str with
    | "refs" :: "heads" :: rem -> String.concat "/" rem
    | _ -> str in
  let fn1 branch = function
    | `Add path -> Fmt.pr "%s: %a %s\n" (short branch) Fmt.(styled (`Fg `Green) string) "+" path
    | `Rem path -> Fmt.pr "%s: %a %s\n" (short branch) Fmt.(styled (`Fg `Red) string) "-" path
    | `Set path -> Fmt.pr "%s: %a %s\n" (short branch) Fmt.(styled (`Fg `Yellow) string) "~" path in
  let fn0 (branch, changes) = List.iter (fn1 branch) changes in
  List.iter fn0 changes

let run _quiet depth remote branch =
  Miou_unix.run ~domains:0 @@ fun () ->
  let* branch = match remote, branch with
    | Some _, Some _ -> error_msgf "Impossible to choose a branch between the given endpoint and the given branch"
    | Some { Mgit_sync.Endpoint.branch= Some branch; _ }, None -> Ok (Some branch)
    | (Some { Mgit_sync.Endpoint.branch= None; _ } | None), branch -> Ok branch in
  let ctx = Flow.ctx ?ssh:(Sys.getenv_opt "MGIT_SSH") () in
  let remote = match remote with
    | Some remote ->
      Git.remote ctx (Mgit_sync.Endpoint.to_string remote)
      |> Result.get_ok |> Option.some
    | None -> None in
  let* git = Git.make ?branch ?depth ~now ?remote Git.Mem in
  let or_report value fn = match value with
    | Ok value -> fn value
    | Error err -> Fmt.epr "error: %a\n%!" Mgit.pp_error err in
  let change git fn = or_report (Git.change_and_push git fn |> Result.join) Fun.id in
  let rec go () =
    match input_line stdin with
    | exception End_of_file -> ()
    | line ->
      let sstr = String.split_on_char ' ' line in
      let sstr = List.drop_while ((=) "") sstr in
      match sstr with
      | [ "pull" ] ->
        begin or_report (Git.pull git) @@ fun changes ->
        print_changes changes end; go ()
      | [ "head" ] ->
        begin match Git.commit git with
        | Some (`Clean uid) -> Fmt.pr "%s\n%!" (Mgit.uid_to_hex uid)
        | Some (`Dirty uid) -> Fmt.pr "~%s\n%!" (Mgit.uid_to_hex uid)
        | None -> Fmt.epr "error: No commit" end;
        go ()
      | "set" :: path :: contents ->
        let contents = String.concat " " contents in
        change git (fun git -> Git.set git path contents);
        go ()
      | "get" :: path ->
        let path = String.concat " " path in
        begin or_report (Git.get git path) @@ fun (_, contents) ->
        Fmt.pr "%s\n%!" contents end; go ()
      | "list" :: path ->
        let path = String.concat " " path in
        begin or_report (Git.list git path) @@ fun entries ->
        let fn (name, k) = match k with
          | `Value -> Fmt.pr "%s\n%!" name
          | `Dictionary -> Fmt.pr "%s/\n%!" name in
        List.iter fn entries end; go ()
      | "gc" :: depth ->
        let depth = String.concat " " depth in
        begin match int_of_string_opt depth with
        | Some n when n > 0 -> or_report (Git.gc git ~shallows:n) Fun.id
        | _ -> Fmt.epr "error: Invalid depth\n%!" end;
        go ()
      | "bundle" :: output ->
        let output = String.concat " " output in
        let oc = open_out_bin output in
        Git.to_bundle git (output_string oc);
        close_out oc; go ()
      | [] -> go ()
      | _ -> Fmt.epr "error: Invalid comment %S\n%!" line; go () in
  go (); Ok ()

open Cmdliner
open Mgit_cli

let term =
  let open Term in
  const run $ setup_logs $ depth $ remote $ branch
  |> map (Result.map_error (msgf "%a" Mgit.pp_error))
  |> term_result ~usage:false

let cmd =
  let doc = "Manipulate a Git repository in memory." in
  let man = [] in
  let info = Cmd.info ~doc ~man "mem" in
  Cmd.v info term
