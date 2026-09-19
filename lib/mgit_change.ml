type read = Carton.Uid.t -> (Carton.Kind.t * string) option
type news = (string, Carton.Kind.t * string) Hashtbl.t
type error = [ `Msg of string ]
type t = string list * [ `Set of Mgit_object.Tree.perm * string | `Rem ]

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let ( let* ) = Result.bind

let make () : news = Hashtbl.create 0x10
let find news uid = Hashtbl.find_opt news (uid : Carton.Uid.t :> string)

let add news kind payload =
  let uid = Mgit_object.digest ~kind payload in
  Hashtbl.replace news (uid :> string) (kind, payload);
  uid

let read_with read news uid =
  match Hashtbl.find_opt news (uid : Carton.Uid.t :> string) with
  | Some _ as value -> value
  | None -> read uid

let empty_tree = Mgit_object.Tree.digest Mgit_object.Tree.empty

let tree read uid =
  if Carton.Uid.equal uid empty_tree then Ok Mgit_object.Tree.empty
  else
    match read uid with
    | Some (`B, payload) -> Mgit_object.Tree.of_string payload
    | Some _ -> error_msgf "%a is not a tree" Mgit_object.pp_uid uid
    | None -> error_msgf "%a is unavailable" Mgit_object.pp_uid uid

let rec apply ~read ~news uid path value =
  let* t = tree read uid in
  match (path, value) with
  | [], _ -> error_msgf "Empty path"
  | [ name ], `Rem ->
      let t = Mgit_object.Tree.remove t name in
      if Mgit_object.Tree.is_empty t then Ok None
      else Ok (Some (add news `B (Mgit_object.Tree.to_string t)))
  | [ name ], `Set (perm, contents) ->
      let node = add news `C contents in
      let t = Mgit_object.Tree.add t { Mgit_object.Tree.perm; name; node } in
      Ok (Some (add news `B (Mgit_object.Tree.to_string t)))
  | name :: rest, _ ->
      let sub =
        match Mgit_object.Tree.find t name with
        | Some { Mgit_object.Tree.perm= `Dir; node; _ } -> node
        | Some _ | None -> empty_tree in
      let* sub = apply ~read ~news sub rest value in
      let t =
        match sub with
        | Some node -> Mgit_object.Tree.add t { Mgit_object.Tree.perm= `Dir; name; node }
        | None -> Mgit_object.Tree.remove t name in
      Ok (Some (add news `B (Mgit_object.Tree.to_string t)))

let root ~read ~news root changes =
  let read = read_with read news in
  let root = Option.value ~default:empty_tree root in
  let rec go root = function
    | [] -> Ok root
    | (path, value) :: rest ->
        let* root = apply ~read ~news root path value in
        let root =
          match root with
          | Some uid -> uid
          | None -> add news `B (Mgit_object.Tree.to_string Mgit_object.Tree.empty)
        in
        go root rest in
  go root changes
