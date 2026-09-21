let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

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
    let normalized = Fpath.of_string str in
    if Result.is_ok normalized && Sys.file_exists str = false
    then Ok str
    else error_msgf "Invalid path: %S" str in
  Arg.conv (parser, Fmt.string)
