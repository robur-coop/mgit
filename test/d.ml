let failwithf fmt = Fmt.failwith fmt
let exitf fmt = Fmt.kstr (fun msg -> Fmt.epr "%s.\n%!" msg; exit 1) fmt
let or_exit ~pp = function Ok v -> v | Error err -> exitf "%a" pp err
let or_failure ~pp = function Ok v -> v | Error err -> failwithf "%a" pp err

let pp_error ppf = function
  | #Mgit.error as err -> Mgit.pp_error ppf err
  | `Invalid_metadata -> Fmt.string ppf "Invalid metadata"

module Block = Mgit_unix.Block
module Git = Mgit_unix

let contents idx =
  let state = ref (idx * 2654435761) in
  String.init 0x40000 @@ fun _ ->
  state := (!state * 1103515245) + 12345;
  Char.unsafe_chr ((!state lsr 16) land 0xff)

let check t =
  let fn (files, counter) ~path _perm str =
    Unix.sleepf 0.02;
    match path with
    | [ "zcounter" ] -> (files + 1, int_of_string str)
    | [ filename ] ->
      let idx = Scanf.sscanf filename "file_%d" Fun.id in
      if not (String.equal str (contents idx))
      then failwithf "/%s has not the expected contents" filename;
      (files + 1, counter)
    | _ -> failwithf "Unexpected %s" (String.concat "/" path) in
  Git.fold t fn (0, -1)

let readers_and_writer (type k) ?(commits= 30) ?(readers= 3) (t : k Git.t) () =
  let ( let* ) = Result.bind in
  let stop = Atomic.make false and started = Atomic.make 0 in
  let reader () =
    let rec go seen =
      if seen = 1 then Atomic.incr started;
      if Atomic.get stop then seen
      else
        let files, counter = check t in
        if (files > 0 || counter >= 0)
        && files <> counter + 1
        then failwithf "counter %d but %d files" counter files;
        Miou.yield ();
        go (succ seen) in
    go 0 in
  let prm0 = Miou.async @@ fun () ->
    while Atomic.get started < readers do Miou.yield () done;
    (* readers are ready to read! *)
    for idx = 1 to commits do
      let result = Git.change_and_push t @@ fun t ->
        let* () = Git.set t "/zcounter" (string_of_int idx) in
        Git.set t (Fmt.str "/file_%d" idx) (contents idx) in
      or_failure ~pp:pp_error (Result.join result)
    done;
    Atomic.set stop true in
  let seen =
    Miou.parallel reader (List.init readers (Fun.const ()))
    |> List.map @@ or_failure ~pp:Fmt.exn in
  Miou.await_exn prm0;
  let fn acc seen = acc && seen > 1 in
  Test.check (List.fold_left fn true seen);
  Test.check (check t = (commits + 1, commits))

let test00 =
  let descr = {text|parallel readers and writer on mem|text} in
  Test.test ~title:"test00" ~descr @@ fun () ->
  Miou_unix.run @@ fun () ->
  let t = or_exit ~pp:pp_error (Git.make ~depth:1 Git.Mem) in
  readers_and_writer t ()

let ( / ) = Filename.concat

let () =
  let tests = [ test00 ] in
  let ({ Test.directory } as runner) = Test.runner (Sys.getcwd () / "_tests") in
  let run idx test =
    Format.printf "test%03d: %!" (succ idx);
    Test.run runner test;
    Format.printf "ok\n%!"
  in
  Format.printf "Run tests into %s\n%!" directory;
  List.iteri run tests
