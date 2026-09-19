type read = Carton.Uid.t -> (Carton.Kind.t * string) option

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

exception Missing of Carton.Uid.t
exception Not_a_commit of Carton.Uid.t

let commits ~read ~depth root =
  let seen = Hashtbl.create 0x10 in
  let kept = ref [] and boundary = ref [] in
  let visited uid =
    let uid = (uid : Carton.Uid.t :> string) in
    if Hashtbl.mem seen uid then true
    else (Hashtbl.replace seen uid (); false) in
  let rec go = function
    | [] -> ()
    | (uid, _) :: rest when visited uid -> go rest
    | (uid, d) :: rest when d > depth -> boundary := uid :: !boundary; go rest
    | (uid, d) :: rest ->
        begin match read uid with
        | None -> boundary := uid :: !boundary; go rest
        | Some (`A, payload) ->
            kept := uid :: !kept;
            let parents = Mgit_object.parents_of_commit payload in
            let parents = List.map (fun uid -> (uid, d + 1)) parents in
            go (rest @ parents)
        | Some _ -> raise (Not_a_commit uid)
        end in
  go [ (root, 1) ];
  (List.rev !kept, List.rev !boundary)

let objects ~read commits =
  let seen = Hashtbl.create 0x100 in
  let trees = ref [] and blobs = ref [] in
  let visited uid =
    let uid = (uid : Carton.Uid.t :> string) in
    if Hashtbl.mem seen uid then true
    else (Hashtbl.replace seen uid (); false) in
  let rec go = function
    | [] -> ()
    | uid :: rest when visited uid -> go rest
    | uid :: rest ->
        begin match read uid with
        | None -> raise (Missing uid)
        | Some (`C, _) -> blobs := uid :: !blobs; go rest
        | Some (`B, payload) ->
            trees := uid :: !trees;
            begin match Mgit_object.Tree.of_string payload with
            | Ok tree ->
                let fn { Mgit_object.Tree.perm; node; _ } =
                  if perm = `Commit then None else Some node in
                go (rest @ List.filter_map fn (Mgit_object.Tree.to_list tree))
            | Error (`Msg _) -> raise (Missing uid)
            end
        | Some ((`A | `D), _) -> raise (Not_a_commit uid)
        end in
  let roots =
    let fn uid =
      match read uid with
      | Some (`A, payload) -> Mgit_object.tree_of_commit payload
      | _ -> None in
    List.filter_map fn commits in
  List.iter (fun uid -> go [ uid ]) roots;
  (List.rev !trees, List.rev !blobs)

let closure ~read ~depth root =
  match
    let kept, boundary = commits ~read ~depth root in
    let trees, blobs = objects ~read kept in
    (kept @ trees @ blobs, boundary)
  with
  | value -> Ok value
  | exception Missing uid ->
      error_msgf "%a is unavailable" Mgit_object.pp_uid uid
  | exception Not_a_commit uid ->
      error_msgf "%a is not the expected kind of object" Mgit_object.pp_uid uid

let uncommon ~read ~exclude root =
  match
    let everything uid =
      let kept, _ = commits ~read ~depth:max_int uid in
      let trees, blobs = objects ~read kept in
      kept @ trees @ blobs in
    let known = Hashtbl.create 0x100 in
    let fn uid =
      if Option.is_some (read uid)
      then
        List.iter
          (fun uid -> Hashtbl.replace known (uid : Carton.Uid.t :> string) ())
          (everything uid) in
    List.iter fn exclude;
    let unknown uid = not (Hashtbl.mem known (uid : Carton.Uid.t :> string)) in
    List.filter unknown (everything root)
  with
  | value -> Ok value
  | exception Missing uid ->
      error_msgf "%a is unavailable" Mgit_object.pp_uid uid
  | exception Not_a_commit uid ->
      error_msgf "%a is not the expected kind of object" Mgit_object.pp_uid uid
