let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

type scheme =
  [ `Git
  | `SSH
  | `HTTP
  | `HTTPS ]

type t =
  { scheme : scheme
  ; user : string option
  ; host : string
  ; port : int option
  ; path : string
  ; branch : string option }

let default_port = function
  | `Git -> 9418
  | `SSH -> 22
  | `HTTP -> 80
  | `HTTPS -> 443

let port t = Option.value ~default:(default_port t.scheme) t.port

let string_of_scheme = function
  | `Git -> "git"
  | `SSH -> "ssh"
  | `HTTP -> "http"
  | `HTTPS -> "https"

let split_branch str =
  match String.rindex_opt str '#' with
  | None -> (str, None)
  | Some idx ->
      let branch = String.sub str (idx + 1) (String.length str - idx - 1) in
      let branch = if branch = "" then None else Some branch in
      (String.sub str 0 idx, branch)

let split_user str =
  match String.index_opt str '@' with
  | None -> (None, str)
  | Some idx ->
      ( Some (String.sub str 0 idx)
      , String.sub str (idx + 1) (String.length str - idx - 1) )

let split_host_port str =
  if String.length str > 0 && str.[0] = '[' then
    match String.index_opt str ']' with
    | None -> error_msgf "Unterminated IPv6 address: %S" str
    | Some idx ->
        let host = String.sub str 1 (idx - 1) in
        let rest = String.sub str (idx + 1) (String.length str - idx - 1) in
        if rest = "" then Ok (host, None)
        else if rest.[0] = ':' then
          match int_of_string_opt (String.sub rest 1 (String.length rest - 1)) with
          | Some port -> Ok (host, Some port)
          | None -> error_msgf "Invalid port in %S" str
        else error_msgf "Invalid authority: %S" str
  else
    match String.rindex_opt str ':' with
    | None -> Ok (str, None)
    | Some idx ->
        let host = String.sub str 0 idx in
        let port = String.sub str (idx + 1) (String.length str - idx - 1) in
        begin match int_of_string_opt port with
        | Some port -> Ok (host, Some port)
        | None -> error_msgf "Invalid port in %S" str
        end

let of_uri ~scheme rest branch =
  let authority, path =
    match String.index_opt rest '/' with
    | None -> (rest, "/")
    | Some idx ->
        (String.sub rest 0 idx, String.sub rest idx (String.length rest - idx))
  in
  let user, authority = split_user authority in
  match split_host_port authority with
  | Error _ as err -> err
  | Ok (host, port) ->
      if host = "" then error_msgf "Missing host in %S" rest
      else Ok { scheme; user; host; port; path; branch }

(* [user@host:path], which is neither a URL nor ambiguous with [host:port]
   because what follows the colon is not a number. *)
let of_scp str branch =
  match String.index_opt str ':' with
  | None -> error_msgf "Invalid endpoint: %S" str
  | Some idx ->
      let authority = String.sub str 0 idx in
      let path = String.sub str (idx + 1) (String.length str - idx - 1) in
      let user, host = split_user authority in
      if host = "" then error_msgf "Missing host in %S" str
      else Ok { scheme= `SSH; user; host; port= None; path; branch }

let of_string str =
  let str, branch = split_branch str in
  let prefix p = String.starts_with ~prefix:p str in
  let strip p = String.sub str (String.length p) (String.length str - String.length p) in
  if prefix "git://" then of_uri ~scheme:`Git (strip "git://") branch
  else if prefix "ssh://" then of_uri ~scheme:`SSH (strip "ssh://") branch
  else if prefix "http://" then of_uri ~scheme:`HTTP (strip "http://") branch
  else if prefix "https://" then of_uri ~scheme:`HTTPS (strip "https://") branch
  else if String.contains str ':' then of_scp str branch
  else error_msgf "Invalid endpoint: %S" str

let to_string t =
  let user = match t.user with Some user -> user ^ "@" | None -> "" in
  let host = if String.contains t.host ':' then "[" ^ t.host ^ "]" else t.host in
  let port = match t.port with Some port -> ":" ^ string_of_int port | None -> "" in
  let path = if String.length t.path > 0 && t.path.[0] = '/' then t.path else "/" ^ t.path in
  let branch = match t.branch with Some branch -> "#" ^ branch | None -> "" in
  Fmt.str "%s://%s%s%s%s%s" (string_of_scheme t.scheme) user host port path branch

let pp ppf t = Fmt.string ppf (to_string t)
