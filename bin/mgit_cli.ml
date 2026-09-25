let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt
let ( let* ) = Result.bind

open Cmdliner

let output_options = "OUTPUT OPTIONS"

let verbosity =
  let env = Cmd.Env.info "MGIT_LOGS" in
  Logs_cli.level ~docs:output_options ~env ()

let renderer =
  let env = Cmd.Env.info "MGIT_FMT" in
  Fmt_cli.style_renderer ~docs:output_options ~env ()

let utf_8 =
  let doc = "Allow binaries to emit UTF-8 characters." in
  let env = Cmd.Env.info "MGIT_UTF_8" in
  Arg.(value & opt bool true & info [ "with-utf-8" ] ~doc ~env)

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      Fmt.kpf k ppf
        ("[%a]%a[%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Cyan int)
        (Stdlib.Domain.self () :> int)
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let setup_logs utf_8 style_renderer level =
  Fmt_tty.setup_std_outputs ~utf_8 ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (reporter Fmt.stderr) ;
  Option.is_none level

let setup_logs = Term.(const setup_logs $ utf_8 $ renderer $ verbosity)

let bytes_of_string s =
  let s = String.trim s in
  let len = String.length s in
  let rec find_non_digit i =
    if i >= len then i
    else if s.[i] >= '0' && s.[i] <= '9' then find_non_digit (i + 1)
    else i
  in
  let idx = find_non_digit 0 in
  let number_str = String.sub s 0 idx |> String.trim in
  let unit_str = String.sub s idx (len - idx) |> String.trim in
  let ( let* ) = Option.bind in
  let* number = int_of_string_opt number_str in
  let* multiplier =
    match String.lowercase_ascii unit_str with
    | "" | "b" -> Some 1
    | "kib" -> Some 1024
    | "mib" -> Some (1024 * 1024)
    | "gib" -> Some (1024 * 1024 * 1024)
    | "tib" -> Some (1024 * 1024 * 1024 * 1024)
    | _ -> None
  in
  Some (number * multiplier)

let sizes = [| "B"; "KiB"; "MiB"; "GiB"; "TiB" |]

let bytes_to_size = function
  | 0 -> "0b"
  | n ->
      let n = float_of_int n in
      let i = Float.floor (Float.log n /. Float.log 1024.) in
      let r = n /. Float.pow 1024. i in
      Fmt.str "%.0f%s" r sizes.(int_of_float i)

let size =
  let parser str =
    match bytes_of_string str with
    | Some n -> Ok n
    | None -> error_msgf "Invalid size: %S" str
  in
  Arg.conv (parser, Fmt.(using bytes_to_size string))

let non_existing_file =
  let parser str =
    match Fpath.of_string str with
    | Ok v when Sys.file_exists str = false -> Ok v
    | Ok v -> error_msgf "Invalid path (it already exists): %a" Fpath.pp v
    | Error _ as err -> err in
  Arg.conv (parser, Fpath.pp)

let existing_file =
  let parser str = match Fpath.of_string str with
    | Ok v when Sys.file_exists str && Sys.is_regular_file str -> Ok v
    | Ok v -> error_msgf "%a is not a file and/or does not exists" Fpath.pp v
    | Error _ as err -> err in
  Arg.conv (parser, Fpath.pp)

let docs_hexdump = "HEX OUTPUT"

let colorscheme =
  let x = Array.make 256 `None in
  for i = 0 to 31 do
    x.(i) <- `Style (`Fg, `bit24 (0xaf, 0xd7, 0xff))
  done;
  for i = 48 to 57 do
    x.(i) <- `Style (`Fg, `bit24 (0xaf, 0xdf, 0x77))
  done;
  for i = 65 to 90 do
    x.(i) <- `Style (`Fg, `bit24 (0xff, 0xaf, 0x5f))
  done;
  for i = 97 to 122 do
    x.(i) <- `Style (`Fg, `bit24 (0xff, 0xaf, 0xd7))
  done;
  Hxd.colorscheme_of_array x

let cols =
  let doc = "Format $(i,COLS) octets per line. Default 16. Max 256." in
  let parser str =
    match int_of_string str with
    | n when n < 1 || n > 256 ->
        error_msgf "Invalid COLS value (must <= 256 && > 0): %d" n
    | n -> Ok n
    | exception _ -> error_msgf "Invalid COLS value: %S" str
  in
  let open Arg in
  let cols = conv (parser, Fmt.int) in
  value
  & opt (some cols) None
  & info [ "c"; "cols" ] ~doc ~docv:"COLS" ~docs:docs_hexdump

let groupsize =
  let doc =
    "Separate the output of every $(i,bytes) bytes (two hex characters) by a \
     whitespace. Specify -g 0 to supress grouping. $(i,bytes) defaults to 2."
  in
  let open Arg in
  value
  & opt (some int) None
  & info [ "g"; "groupsize" ] ~doc ~docv:"BYTES" ~docs:docs_hexdump

let len =
  let doc = "Stop after writing $(i,LEN) octets." in
  let open Arg in
  value
  & opt (some int) None
  & info [ "l"; "len" ] ~doc ~docv:"LEN" ~docs:docs_hexdump

let uppercase =
  let doc = "Use upper case hex letters. Default is lower case." in
  let open Arg in
  value & flag & info [ "u" ] ~doc ~docs:docs_hexdump

let setup_hxd cols groupsize len uppercase =
  Hxd.xxd ?cols ?groupsize ?long:len ~uppercase colorscheme

let setup_hxd = Term.(const setup_hxd $ cols $ groupsize $ len $ uppercase)

module Block = Mgit_unix.Block
module Flow = Mgit_unix.Flow
module Git = Mgit_unix

let ssh =
  let doc = "The $(b,ssh) executable we use for SSH transports." in
  Cmd.Env.info "MGIT_SSH" ~doc

let setup_context () =
  let ssh = Sys.getenv_opt "MGIT_SSH" in
  Flow.ctx ?ssh:ssh ()

let setup_context =
  let open Term in
  const setup_context $ const ()

let date =
  let doc = "The $(b,epoch) time used for commits." in
  Cmd.Env.info "MGIT_DATE" ~doc

let now = match Sys.getenv_opt "MGIT_DATE" with
  | Some ts -> begin match int_of_string_opt ts with
    | Some ts when ts >= 0 -> Fun.const ts
    | Some _ | None ->
      Logs.warn (fun m -> m "The given MGIT_DATE is wrong (it must a non-negative number)");
      fun () -> int_of_float (Unix.time ()) end
  | None -> fun () -> int_of_float (Unix.time ())

let setup_git ctx depth remote branch filename =
  let* branch = match remote, branch with
    | Some _, Some _ -> error_msgf "Impossible to choose a branch between the given endpoint and the given branch"
    | Some { Mgit_sync.Endpoint.branch= Some branch; _ }, None -> Ok (Some branch)
    | (Some { Mgit_sync.Endpoint.branch= None; _ } | None), branch -> Ok branch in
  let remote = match remote with
    | Some remote ->
      Git.remote ctx (Mgit_sync.Endpoint.to_string remote)
      |> Result.get_ok |> Option.some
    | None -> None in
  let* blk = Block.load (Fpath.to_string filename) in
  Git.make ?branch ?depth ~now ?remote (Git.Blk blk)

let non_negative_int =
  let parser str = match int_of_string_opt str with
    | Some n when n > 0 -> Ok n
    | Some _ -> error_msgf "A depth must be greater than 0"
    | None -> error_msgf "Invalid depth: %S" str in
  Arg.conv (parser, Fmt.int)

let depth =
  let doc = "The depth of the Git history." in
  let env = Cmd.Env.info "MGIT_DEPTH" in
  let open Arg in
  value & opt (some non_negative_int) (Some 1) & info [ "depth" ] ~doc ~env ~docv:"DEPTH"

let endpoint =
  let parser = Mgit_sync.Endpoint.of_string in
  let pp = Mgit_sync.Endpoint.pp in
  Arg.conv (parser, pp)

let remote =
  let doc = "The remote Git repository." in
  let open Arg in
  value & opt (some endpoint) None & info [ "r"; "remote" ] ~doc ~docv:"REMOTE"

let branch =
  let doc = "The branch of the Git repository." in
  let open Arg in
  value & opt (some string) None & info [ "b"; "branch" ] ~doc ~docv:"BRANCH"

let filename =
  let doc = "The block-device as a file to load." in
  let open Arg in
  required & pos 0 (some existing_file) None & info [] ~doc ~docv:"FILENAME"

let absolute_path =
  let parser str = match Fpath.of_string str with
    | Ok v when Fpath.is_abs v -> Ok str
    | Ok _ -> error_msgf "The given path must be absolute and represent a file (an edge)"
    | Error _ as err -> err in
  Arg.conv (parser, Fmt.string)

let path =
  let doc = "A path into the given Git repository." in
  let open Arg in
  required & pos 1 (some absolute_path) None & info [] ~doc ~docv:"PATH"

let absolute_path_for_entry =
  let parser str = match Fpath.of_string str with
    | Ok v when Fpath.is_abs v && Fpath.is_file_path v -> Ok str
    | Ok _ -> error_msgf "The given path must be absolute and represent a file (an edge)"
    | Error _ as err -> err in
  Arg.conv (parser, Fmt.string)

let entry =
  let doc = "The path of the entry into a Git repository." in
  let open Arg in
  required & pos 1 (some absolute_path_for_entry) None & info [] ~doc ~docv:"PATH"

let setup_git =
  let open Term in
  const setup_git $ setup_context $ depth $ remote $ branch $ filename
  |> map (Result.map_error (msgf "%a" Mgit.pp_error))
  |> term_result ~usage:false
