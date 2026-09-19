(* NOTE(dinosaure): ocaml-git for a poor man... *)
(* NOTE(dinosaure): One lesson learned from ocaml-git is that it isn't really
   necessary to define an abstract [type t] for all the Git objects you're
   working with; instead, it's better to be a bit more direct and extract the
   information you need from the objects as they exist in memory or on the
   block device. *)

module SHA1 = Digestif.SHA1

type error = [ `Msg of string ]

let pp_error ppf = function `Msg msg -> Fmt.string ppf msg
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let ( let* ) = Result.bind
let ref_length = SHA1.digest_size

let uid_of_hex hex =
  match Ohex.decode hex with
  | str when String.length str = ref_length ->
      Ok (Carton.Uid.unsafe_of_string str)
  | _ -> error_msgf "Invalid object identifier: %S" hex
  | exception _ -> error_msgf "Invalid object identifier: %S" hex

let uid_of_hex_exn hex =
  match uid_of_hex hex with
  | Ok uid -> uid
  | Error (`Msg msg) -> invalid_arg msg

let hex_of_uid (uid : Carton.Uid.t) = Ohex.encode (uid :> string)
let pp_uid ppf uid = Fmt.string ppf (hex_of_uid uid)
let decode_opt str = try Some (Ohex.decode str) with _exn -> None

let string_of_kind = function
  | `A -> "commit"
  | `B -> "tree"
  | `C -> "blob"
  | `D -> "tag"

let kind_of_string = function
  | "commit" -> Ok `A
  | "tree" -> Ok `B
  | "blob" -> Ok `C
  | "tag" -> Ok `D
  | str -> error_msgf "Invalid object kind: %S" str

(* The Git object identifier: SHA-1 of the {i loose} framing
   ["<kind> <length>\000"] followed by the payload — exactly what
   [git hash-object] computes. *)
let digest ~kind payload =
  let hdr = Fmt.str "%s %d\000" (string_of_kind kind) (String.length payload) in
  let ctx = SHA1.feed_string SHA1.empty hdr in
  let ctx = SHA1.feed_string ctx payload in
  Carton.Uid.unsafe_of_string (SHA1.to_raw_string (SHA1.get ctx))

let identify =
  let init kind (len : Carton.Size.t) =
    let hdr = Fmt.str "%s %d\000" (string_of_kind kind) (len :> int) in
    SHA1.feed_string SHA1.empty hdr in
  let feed bstr ctx = SHA1.feed_bigstring ctx bstr in
  let serialize ctx =
    Carton.Uid.unsafe_of_string (SHA1.to_raw_string (SHA1.get ctx)) in
  { Carton.First_pass.init; feed; serialize }

let digest_pack () =
  let feed_bytes buf ~off ~len ctx = SHA1.feed_bytes ctx ~off ~len buf in
  let feed_bigstring bstr ctx = SHA1.feed_bigstring ctx bstr in
  let serialize ctx = SHA1.to_raw_string (SHA1.get ctx) in
  let hash =
    { Carton.First_pass.feed_bytes; feed_bigstring; serialize
    ; length = SHA1.digest_size } in
  Carton.First_pass.Digest (hash, SHA1.empty)

(* Users *)

module User = struct
  type t = { name : string; email : string; date : int * int option }

  let string_of_tz = function
    | None -> "+0000"
    | Some minutes ->
        let sign = if minutes < 0 then '-' else '+' in
        let minutes = abs minutes in
        Fmt.str "%c%02d%02d" sign (minutes / 60) (minutes mod 60)

  let to_string { name; email; date = secs, tz } =
    Fmt.str "%s <%s> %d %s" name email secs (string_of_tz tz)

  let tz_of_string str =
    if String.length str <> 5 then None
    else
      let sign = match str.[0] with '-' -> Some (-1) | '+' -> Some 1 | _ -> None in
      match
        ( sign
        , int_of_string_opt (String.sub str 1 2)
        , int_of_string_opt (String.sub str 3 2) )
      with
      | Some sign, Some hh, Some mm -> Some (sign * ((hh * 60) + mm))
      | _ -> None

  let of_string str =
    match (String.index_opt str '<', String.rindex_opt str '>') with
    | Some i, Some j when i < j ->
        let name = String.trim (String.sub str 0 i) in
        let email = String.sub str (i + 1) (j - i - 1) in
        let rest = String.sub str (j + 1) (String.length str - j - 1) in
        let fn = ( <> ) "" in
        let rest = List.filter fn (String.split_on_char ' ' (String.trim rest)) in
        begin match rest with
        | [] -> Ok { name; email; date = (0, None) }
        | secs :: rest ->
            begin match int_of_string_opt secs with
            | None -> error_msgf "Invalid date in %S" str
            | Some secs ->
                let tz = match rest with tz :: _ -> tz_of_string tz | [] -> None in
                Ok { name; email; date = (secs, tz) }
            end
        end
    | _ -> error_msgf "Invalid user: %S" str

  let pp ppf t = Fmt.string ppf (to_string t)
end

(* Trees *)

module Tree = struct
  type perm =
    [ `Normal
    | `Exec
    | `Everybody
    | `Link
    | `Dir
    | `Commit ]

  type entry = { perm : perm; name : string; node : Carton.Uid.t }
  type t = entry list (* kept sorted, see [order] *)

  let int_of_perm = function
    | `Normal -> 0o100644
    | `Exec -> 0o100755
    | `Everybody -> 0o100664
    | `Link -> 0o120000
    | `Dir -> 0o040000
    | `Commit -> 0o160000

  let perm_of_int = function
    | 0o100644 -> Ok `Normal
    | 0o100755 -> Ok `Exec
    | 0o100664 -> Ok `Everybody
    | 0o120000 -> Ok `Link
    | 0o040000 -> Ok `Dir
    | 0o160000 -> Ok `Commit
    | perm -> error_msgf "Invalid permission: %o" perm

  let key { perm; name; _ } =
    match perm with `Dir -> name ^ "/" | _ -> name

  let order a b = String.compare (key a) (key b)
  let empty = []
  let v entries = List.sort_uniq order entries
  let to_list t = t
  let is_empty t = t = []
  let find t name = List.find_opt (fun entry -> entry.name = name) t

  let remove t name = List.filter (fun entry -> entry.name <> name) t
  let add t entry = List.merge order [ entry ] (remove t entry.name)
  let uids t = List.map (fun { node; _ } -> node) t

  let of_string str =
    let rec go acc pos =
      if pos >= String.length str then Ok (v (List.rev acc))
      else
        match String.index_from_opt str pos '\000' with
        | None -> error_msgf "Malformed tree object"
        | Some nul ->
            if nul + ref_length >= String.length str
            then error_msgf "Malformed tree object"
            else begin
              match String.index_from_opt str pos ' ' with
              | Some sp when sp < nul ->
                  let perm = String.sub str pos (sp - pos) in
                  let name = String.sub str (sp + 1) (nul - sp - 1) in
                  let node = String.sub str (nul + 1) ref_length in
                  let node = Carton.Uid.unsafe_of_string node in
                  begin match int_of_string_opt ("0o" ^ perm) with
                  | None -> error_msgf "Invalid permission %S" perm
                  | Some perm ->
                      let* perm = perm_of_int perm in
                      go ({ perm; name; node } :: acc) (nul + 1 + ref_length)
                  end
              | _ -> error_msgf "Malformed tree object"
            end in
    go [] 0

  let to_string t =
    let buf = Buffer.create 0x100 in
    let fn { perm; name; node } =
      Buffer.add_string buf (Fmt.str "%o" (int_of_perm perm));
      Buffer.add_char buf ' ';
      Buffer.add_string buf name;
      Buffer.add_char buf '\000';
      Buffer.add_string buf (node :> string) in
    List.iter fn t; Buffer.contents buf

  let digest t = digest ~kind:`B (to_string t)
end

(* Headers, kind of similar to RFC 822 *)

let headers_of_string str =
  let hdrs, message =
    let rec go pos =
      match String.index_from_opt str pos '\n' with
      | None -> (String.sub str 0 (String.length str), None)
      | Some nl when nl = pos ->
          let msg = String.sub str (nl + 1) (String.length str - nl - 1) in
          (String.sub str 0 pos, Some msg)
      | Some nl -> go (nl + 1) in
    go 0 in
  let lines =
    if hdrs = "" then []
    else
     let trimmed =
       if String.length hdrs > 0
       && hdrs.[String.length hdrs - 1] = '\n'
       then String.sub hdrs 0 (String.length hdrs - 1)
       else hdrs in
     String.split_on_char '\n' trimmed in
  let rec fold acc = function
    | [] -> List.rev acc
    | line :: rest when String.length line > 0 && line.[0] = ' ' ->
        let value = String.sub line 1 (String.length line - 1) in
        begin match acc with
        | (key, values) :: acc -> fold ((key, value :: values) :: acc) rest
        | [] -> fold [ ("", [ value ]) ] rest
        end
    | line :: rest ->
        begin match String.index_opt line ' ' with
        | None -> fold ((line, []) :: acc) rest
        | Some sp ->
            let key = String.sub line 0 sp in
            let value = String.sub line (sp + 1) (String.length line - sp - 1) in
            fold ((key, [ value ]) :: acc) rest
        end in
  let hdrs = fold [] lines in
  (List.map (fun (key, values) -> (key, List.rev values)) hdrs, message)

let string_of_headers hdrs =
  let buf = Buffer.create 0x100 in
  let fn (key, values) =
    match values with
    | [] -> Buffer.add_string buf (key ^ "\n")
    | value :: rest ->
        Buffer.add_string buf (key ^ " " ^ value ^ "\n");
        List.iter (fun value -> Buffer.add_string buf (" " ^ value ^ "\n")) rest
  in
  List.iter fn hdrs; Buffer.contents buf

let single key hdrs =
  match List.assoc_opt key hdrs with
  | Some [ value ] -> Ok value
  | Some _ -> error_msgf "Multiple %S headers" key
  | None -> error_msgf "Missing %S header" key

(* Commits *)

module Commit = struct
  type t =
    { tree : Carton.Uid.t
    ; parents : Carton.Uid.t list
    ; author : User.t
    ; committer : User.t
    ; extra : (string * string list) list
    ; message : string option }

  let make ~tree ?(parents = []) ~author ~committer ?(extra = []) message =
    { tree; parents; author; committer; extra; message }

  let reserved = [ "tree"; "parent"; "author"; "committer" ]

  let of_string str =
    let hdrs, message = headers_of_string str in
    let* tree = single "tree" hdrs in
    let* tree = uid_of_hex tree in
    let parents =
      List.filter_map
        (function
          | "parent", [ hex ] -> Result.to_option (uid_of_hex hex)
          | _ -> None)
        hdrs in
    let* author = single "author" hdrs in
    let* author = User.of_string author in
    let* committer = single "committer" hdrs in
    let* committer = User.of_string committer in
    let extra =
      List.filter (fun (key, _) -> not (List.mem key reserved)) hdrs in
    Ok { tree; parents; author; committer; extra; message }

  let to_string { tree; parents; author; committer; extra; message } =
    let hdrs =
      [ ("tree", [ hex_of_uid tree ]) ]
      @ List.map (fun uid -> ("parent", [ hex_of_uid uid ])) parents
      @ [ ("author", [ User.to_string author ])
        ; ("committer", [ User.to_string committer ]) ]
      @ extra in
    let hdrs = string_of_headers hdrs in
    match message with None -> hdrs | Some message -> hdrs ^ "\n" ^ message

  let digest t = digest ~kind:`A (to_string t)
end

(* Tags *)

module Tag = struct
  type t =
    { obj : Carton.Uid.t
    ; kind : Carton.Kind.t
    ; tag : string
    ; tagger : User.t option
    ; message : string option }

  let of_string str =
    let hdrs, message = headers_of_string str in
    let* obj = single "object" hdrs in
    let* obj = uid_of_hex obj in
    let* kind = single "type" hdrs in
    let* kind = kind_of_string kind in
    let* tag = single "tag" hdrs in
    let* tagger =
      match List.assoc_opt "tagger" hdrs with
      | Some [ tagger ] ->
          let* tagger = User.of_string tagger in
          Ok (Some tagger)
      | _ -> Ok None in
    Ok { obj; kind; tag; tagger; message }

  let to_string { obj; kind; tag; tagger; message } =
    let hdrs =
      [ ("object", [ hex_of_uid obj ])
      ; ("type", [ string_of_kind kind ])
      ; ("tag", [ tag ]) ]
      @ (match tagger with
        | Some tagger -> [ ("tagger", [ User.to_string tagger ]) ]
        | None -> []) in
    let hdrs = string_of_headers hdrs in
    match message with None -> hdrs | Some message -> hdrs ^ "\n" ^ message

  let digest t = digest ~kind:`D (to_string t)
end

(* Fast decoders *)

(* tree <hex>\n *)
let tree_of_commit str =
  match String.index_opt str '\n' with
  | None -> None
  | Some eol ->
      begin match String.split_on_char ' ' (String.sub str 0 eol) with
      | [ "tree"; hex ] ->
          decode_opt hex |> Option.map Carton.Uid.unsafe_of_string
      | _ -> None
      end

(* parent <hex>\n, right after the [tree] line *)
let parents_of_commit str =
  let rec go acc pos =
    match String.index_from_opt str pos '\n' with
    | None -> List.rev acc
    | Some eol ->
        begin match String.split_on_char ' ' (String.sub str pos (eol - pos)) with
        | [ "parent"; hex ] ->
            begin match decode_opt hex with
            | Some uid -> go (Carton.Uid.unsafe_of_string uid :: acc) (eol + 1)
            | None -> List.rev acc
            end
        | [ "tree"; _ ] -> go acc (eol + 1)
        | _ -> List.rev acc
        end in
  go [] 0

(* object <hex>\n *)
let target_of_tag str =
  match String.index_opt str '\n' with
  | None -> None
  | Some eol ->
      begin match String.split_on_char ' ' (String.sub str 0 eol) with
      | [ "object"; hex ] ->
          decode_opt hex |> Option.map Carton.Uid.unsafe_of_string
      | _ -> None
      end

let entries str = Result.map Tree.to_list (Tree.of_string str)

let links ~kind str =
  match kind with
  | `A ->
      let tree = Option.to_list (tree_of_commit str) in
      tree @ parents_of_commit str
  | `B -> begin match Tree.of_string str with
          | Ok tree -> Tree.uids tree
          | Error _ -> []
          end
  | `C -> []
  | `D -> Option.to_list (target_of_tag str)
