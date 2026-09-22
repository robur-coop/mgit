let exitf fmt = Fmt.kstr (fun msg -> Fmt.epr "%s.\n%!" msg; exit 1) fmt
let or_exit ~pp = function Ok v -> v | Error err -> exitf "%a" pp err
let tmpf ?(ext= ".img") fmt = Fmt.kstr (fun str -> Filename.temp_file str ext) fmt

module Block = Mgit_unix.Block
module Blk = Mgit_blk.Make (Block)

let fill t which str =
  let sink = Blk.sink t which in
  Flux.(Stream.into sink (Stream.from (Source.list [ str ])))

let read t which ~len =
  Blk.seq t which ~len ()
  |> List.of_seq
  |> String.concat ""

let pp_error ppf = function
  | `Msg msg -> Fmt.string ppf msg
  | `Invalid_metadata -> Fmt.string ppf "Invalid metadata"

let rd bstr = Ok (Int64.to_int (Bstr.get_int64_le bstr 0))
let wr v bstr = Bstr.set_int64_le bstr 0 (Int64.of_int v); 8

let test00 =
  let descr = {text|zone are disjoint|text} in
  Test.test ~title:"test00" ~descr @@ fun () ->
  let filename = tmpf "mgit" in
  let sector_size = 0x200
  and length = 0x100_000 in
  let blk = or_exit ~pp:pp_error (Block.create ~sector_size filename length) in
  let finally () = Block.close blk in
  Fun.protect ~finally @@ fun () ->
  let t = or_exit ~pp:pp_error (Blk.format ~rd ~wr blk) in
  fill t `Temporary "tmp";
  fill t `Active "foo";
  fill t `Inactive "bar";
  Test.check (read t `Temporary ~len:3 = "tmp");
  Test.check (read t `Active ~len:3 = "foo");
  Test.check (read t `Inactive ~len:3 = "bar")

let test01 =
  let descr = {text|swap|text} in
  Test.test ~title:"test02" ~descr @@ fun () ->
  let filename = tmpf "mgit" in
  let sector_size = 0x200
  and length = 0x100_000 in
  let blk = or_exit ~pp:pp_error (Block.create ~sector_size filename length) in
  let t = or_exit ~pp:pp_error (Blk.format ~rd ~wr blk) in
  fill t `Active "old";
  fill t `Inactive "new";
  let t = Blk.commit t in
  Test.check (read t `Active ~len:3 = "new");
  Test.check (read t `Inactive ~len:3 = "old");
  Block.close blk;
  let blk = or_exit ~pp:pp_error (Block.load ~sector_size filename) in
  let t = or_exit ~pp:pp_error (Blk.make ~rd ~wr blk) in
  Test.check (read t `Active ~len:3 = "new");
  Test.check (read t `Inactive ~len:3 = "old");
  Block.close blk

let test02 =
  let descr = {text|uncommitted|text} in
  Test.test ~title:"test03" ~descr @@ fun () ->
  let filename = tmpf "mgit" in
  let sector_size = 0x200
  and length = 0x100_000 in
  let blk = or_exit ~pp:pp_error (Block.create ~sector_size filename length) in
  let t = or_exit ~pp:pp_error (Blk.format ~rd ~wr blk) in
  fill t `Active "commit";
  let t = Blk.sync t in
  fill t `Inactive "half";
  Block.close blk;
  let blk = or_exit ~pp:pp_error (Block.load ~sector_size filename) in
  let t = or_exit ~pp:pp_error (Blk.make ~rd ~wr blk) in
  Test.check (read t `Active ~len:6 = "commit");
  Block.close blk

let test03 =
  let descr = {text|metadata|text} in
  Test.test ~title:"test03" ~descr @@ fun () ->
  let filename = tmpf "mgit" in
  let sector_size = 0x200
  and length = 0x100_000 in
  let blk = or_exit ~pp:pp_error (Block.create ~sector_size filename length) in
  let t = or_exit ~pp:pp_error (Blk.format ~rd ~wr blk) in
  Test.check (Blk.metadata t = 0);
  let _ = Blk.sync (Blk.with_metadata t 42) in
  Block.close blk;
  let blk = or_exit ~pp:pp_error (Block.load ~sector_size filename) in
  let t = or_exit ~pp:pp_error (Blk.make ~rd ~wr blk) in
  Test.check (Blk.metadata t = 42);
  Block.close blk

let test04 =
  let descr = {text|bound|text} in
  Test.test ~title:"test04" ~descr @@ fun () ->
  let filename = tmpf "mgit" in
  let sector_size = 0x200
  and length = 0x100_000 in
  let blk = or_exit ~pp:pp_error (Block.create ~sector_size filename length) in
  let t = or_exit ~pp:pp_error (Blk.format ~rd ~wr blk) in
  fill t `Active (String.make 4096 'x');
  let cache = Blk.cachet t `Active ~base:0 ~len:10 in
  Test.check (Cachet.get_string cache ~len:5 0 = "xxxxx");
  let bstr = Cachet.map cache ~pos:(10 * sector_size) sector_size in
  Test.check (Cachet.Bstr.length bstr = 0);
  Block.close blk

let ( / ) = Filename.concat

let () =
  let tests = [ test00; test01; test02; test03; test04 ] in
  let ({ Test.directory } as runner) = Test.runner (Sys.getcwd () / "_tests") in
  let run idx test =
    Format.printf "test%03d: %!" (succ idx);
    Test.run runner test;
    Format.printf "ok\n%!"
  in
  Format.printf "Run tests into %s\n%!" directory;
  List.iteri run tests
