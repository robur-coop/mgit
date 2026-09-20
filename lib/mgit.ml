module Blk = Mgit_blk
module Mem = Mgit_mem
module Sync = Mgit_sync
module Change = Mgit_change
module Object = Mgit_object
module Closure = Mgit_closure

let src = Logs.Src.create "mgit"

module Log = (val Logs.src_log src : Logs.LOG)

type error =
  [ `Msg of string
  | `Out_of_space
  | `Not_found of string ]

let pp_error ppf = function
  | `Msg msg -> Fmt.string ppf msg
  | `Out_of_space -> Fmt.string ppf "Zone of the block-device full"
  | `Not_found path -> Fmt.pf ppf "%s not found" path

type perm = [ `Normal | `Exec | `Everybody | `Link ]
type change = [ `Add of string | `Rem of string | `Set of string ]
type uid = Carton.Uid.t

let uid_of_hex hex =
  match Mgit_object.uid_of_hex hex with
  | Ok uid -> Ok uid
  | Error _ -> Error (`Msg (Fmt.str "Invalid identifier: %S" hex))

let uid_to_hex uid = Fmt.str "%a" Mgit_object.pp_uid uid

let segments path =
  List.filter (( <> ) "") (String.split_on_char '/' path)

let reference branch =
  if String.starts_with ~prefix:"refs/" branch then branch
  else "refs/heads/" ^ branch

let author_of ~now ~author ~email =
  let name = Option.value ~default:"mgit" author in
  let email = Option.value ~default:"mgit@uniker.nl" email in
  { Mgit_object.User.name; email; date= (now (), None) }

module Make (Block : Blk.BLOCK) (Flow : Sync.S) = struct
  module Store = Store.Make (Block)
  include Sync.Make (Flow)

  (* NOTE(dinosaure): [located] from [Sync] is renamed to [received]. *)
  type 'tmp received = 'tmp located

  type mem = [ `Mem ]
  type blk = [ `Blk ]

  type 'k from = Blk : Block.t -> blk from | Mem : mem from
  type 'k store =
    | Blk_store : Store.t -> blk store
    | Mem_store : Mgit_mem.t -> mem store

  type changes = Open of Change.t list | Closed
  type staged = { news : Change.news; changes : changes Atomic.t }

  type 'k shared =
    { store : 'k store Atomic.t
    ; mutable depth : int
    ; lock : Miou.Mutex.t }

  type 'k t =
    { shared : 'k shared
    ; branch : string Atomic.t
    ; pinned : 'k store option
    ; now : unit -> int
    ; remote : remote option
    ; staged : staged option }

  let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
  let ( let* ) = Result.bind

  let store_of t =
    match t.pinned with
    | Some store -> store
    | None -> Atomic.get t.shared.store

  let remote ctx str =
    let* edn = Mgit_sync.Endpoint.of_string str in
    Ok { ctx; edn }

  let format ?ratio ?length block =
    match Store.format ?ratio ?length block with
    | Ok () -> Ok ()
    | Error (`Msg _ as err) -> Error err
    | Error (`Out_of_space | `Not_found _) -> error_msgf "Invalid block-device"

  let make :
      type k.
         ?branch:string
      -> ?depth:int
      -> ?now:(unit -> int)
      -> ?remote:remote
      -> k from
      -> (k t, [> error ]) result =
   fun ?(branch = "refs/heads/main") ?(depth = 1) ?(now = Fun.const 0) ?remote
       from ->
    let branch =
      match remote with
      | Some { edn= { Mgit_sync.Endpoint.branch= Some branch; _ }; _ } -> reference branch
      | _ -> reference branch in
    let v store references =
      let branch =
        match references with (name, _) :: _ -> name | [] -> branch in
      let shared = { store= Atomic.make store; depth; lock= Miou.Mutex.create () } in
      { shared; branch= Atomic.make branch; pinned= None; now; remote; staged= None } in
    match from with
    | Blk block ->
        begin match Store.load block with
        | Error (`Msg _ as err) -> Error err
        | Error (`Out_of_space | `Not_found _) ->
            error_msgf "Invalid block-device"
        | Ok store -> Ok (v (Blk_store store) (Store.references store))
        end
    | Mem -> Ok (v (Mem_store (Mgit_mem.make ())) [])

  (* Unsafe *)

  let unsafe_read : type k. k t -> uid -> (Carton.Kind.t * string) option =
   fun t uid ->
    match store_of t with
    | Blk_store store -> Store.read store uid
    | Mem_store store -> Mgit_mem.read store uid

  let unsafe_references : type k. k t -> (string * uid) list =
   fun t ->
    match store_of t with
    | Blk_store store -> Store.references store
    | Mem_store store -> Mgit_mem.references store

  let branch t = Atomic.get t.branch
  let unsafe_head t = List.assoc_opt (branch t) (unsafe_references t)

  let unsafe_is_empty : type k. k t -> bool =
   fun t ->
    match store_of t with
    | Blk_store store -> Store.is_empty store
    | Mem_store store -> Mgit_mem.is_empty store

  let unsafe_commit t =
    match (unsafe_head t, t.staged) with
    | None, _ -> None
    | Some uid, None -> Some (`Clean uid)
    | Some uid, Some _ -> Some (`Dirty uid)

  let unsafe_tree_root t =
    match unsafe_head t with
    | None -> None
    | Some uid ->
        begin match unsafe_read t uid with
        | Some (`A, payload) -> Mgit_object.tree_of_commit payload
        | _ -> None
        end

  let unsafe_resolve t path =
    let rec go uid = function
      | [] -> Some (`Dir, uid)
      | name :: rest ->
          begin match unsafe_read t uid with
          | Some (`B, payload) ->
              begin match Mgit_object.Tree.of_string payload with
              | Error (`Msg _) -> None
              | Ok tree ->
                  begin match Mgit_object.Tree.find tree name with
                  | None -> None
                  | Some { Mgit_object.Tree.perm; node; _ } ->
                      if rest = [] then Some (perm, node) else go node rest
                  end
              end
          | _ -> None
          end in
    match unsafe_tree_root t with
    | None -> None
    | Some root -> go root path

  let unsafe_get t path =
    match unsafe_resolve t (segments path) with
    | None -> Error (`Not_found path)
    | Some (((`Normal | `Exec | `Everybody | `Link) as perm), uid) ->
        begin match unsafe_read t uid with
        | Some (`C, contents) -> Ok (perm, contents)
        | _ -> error_msgf "%s is not a file" path
        end
    | Some ((`Dir | `Commit), _) -> error_msgf "%s is not a file" path

  let unsafe_list t path =
    match unsafe_resolve t (segments path) with
    | None -> Error (`Not_found path)
    | Some (_, uid) ->
        begin match unsafe_read t uid with
        | Some (`B, payload) ->
            begin match Mgit_object.Tree.of_string payload with
            | Error (`Msg _ as err) -> Error err
            | Ok tree ->
                let fn { Mgit_object.Tree.perm; name; _ } =
                  match perm with
                  | `Dir -> (name, `Dictionary)
                  | _ -> (name, `Value) in
                Ok (List.map fn (Mgit_object.Tree.to_list tree))
            end
        | _ -> error_msgf "%s is not a directory" path
        end

  let unsafe_fold t fn acc =
    let rec go acc rev_path uid =
      match unsafe_read t uid with
      | Some (`B, payload) ->
          begin match Mgit_object.Tree.of_string payload with
          | Error (`Msg _) -> acc
          | Ok tree ->
              let each acc { Mgit_object.Tree.perm; name; node } =
                match perm with
                | `Dir -> go acc (name :: rev_path) node
                | `Commit -> acc (* a submodule *)
                | (`Normal | `Exec | `Everybody | `Link) as perm ->
                    begin match unsafe_read t node with
                    | Some (`C, contents) ->
                        fn acc ~path:(List.rev (name :: rev_path)) perm contents
                    | _ -> acc
                    end in
              List.fold_left each acc (Mgit_object.Tree.to_list tree)
          end
      | _ -> acc in
    match unsafe_tree_root t with
    | None -> acc
    | Some root -> go acc [] root

  (* Publishing a new generation *)

  type meta =
    | New : Carton.Kind.t * string -> meta
    | Old : uid -> meta
    | Tmp : 'fd Carton.t * int -> meta

  let value_of_cursor carton ~cursor =
    let size = Carton.size_of_offset carton ~cursor Carton.Size.zero in
    let blob = Carton.Blob.make ~size in
    Carton.of_offset carton blob ~cursor

  let loader : type k. k t -> uid -> meta -> Carton.Value.t =
   fun t _uid meta ->
    match (store_of t, meta) with
    | _, New (kind, payload) -> Carton.Value.of_string ~kind payload
    | _, Tmp (carton, cursor) -> value_of_cursor carton ~cursor
    | Blk_store store, Old uid -> Option.get (Store.value store uid)
    | Mem_store store, Old uid -> Option.get (Mgit_mem.value store uid)

  type located = { kind : Carton.Kind.t; length : int; meta : meta }

  let entries : type k.
      k t -> lookup:(uid -> located option) -> Change.news -> uid list
      -> meta Cartonnage.Entry.t list =
   fun t ~lookup news uids ->
    let stored uid =
      match store_of t with
      | Blk_store store -> (Store.kind store uid, Store.length store uid)
      | Mem_store store -> (Mgit_mem.kind store uid, Mgit_mem.length store uid) in
    let fn uid =
      match Change.find news uid with
      | Some (kind, payload) ->
          Cartonnage.Entry.make ~kind ~length:(String.length payload) uid
            (New (kind, payload))
      | None ->
          begin match lookup uid with
          | Some { kind; length; meta } ->
              Cartonnage.Entry.make ~kind ~length uid meta
          | None ->
              let kind, length = stored uid in
              Cartonnage.Entry.make ~kind:(Option.get kind)
                ~length:(Option.get length) uid (Old uid)
          end in
    List.map fn uids

  let publish : type k.
      k t -> references:(string * uid) list
      -> prerequisites:(uid * string option) list
      -> lookup:(uid -> located option) -> Change.news -> uid list
      -> (unit, [> error ]) result =
   fun t ~references ~prerequisites ~lookup news uids ->
    let entries = entries t ~lookup news uids in
    let load = loader t in
    match store_of t with
    | Blk_store store ->
        let result = Store.publish store ~references ~prerequisites ~load entries in
        begin match result with
        | Ok store -> Atomic.set t.shared.store (Blk_store store); Ok ()
        | Error (`Not_found uid) ->
            error_msgf "%a is unavailable" Mgit_object.pp_uid uid
        | Error (`Msg _ | `Out_of_space) as err -> err
        end
    | Mem_store store ->
        let fn entry =
          let uid = Cartonnage.Entry.uid entry in
          let value = load uid (Cartonnage.Entry.meta entry) in
          let len = Carton.Value.length value in
          let payload = Bstr.sub_string (Carton.Value.bigstring value) ~off:0 ~len in
          (uid, Carton.Value.kind value, payload) in
        let store =
          Mgit_mem.publish store ~references ~prerequisites (List.map fn entries) in
        Atomic.set t.shared.store (Mem_store store);
        Ok ()

  let generation t ?(depth = t.shared.depth) ?(lookup = Fun.const None) ?reader
      ~news references =
    let reader = Option.value ~default:(unsafe_read t) reader in
    let read = Change.read_with reader news in
    let* uids, boundary = Closure.closure ~read ~depth (List.map snd references) in
    let prerequisites = List.map (fun uid -> (uid, None)) boundary in
    publish t ~references ~prerequisites ~lookup news uids

  let apply t ?author ?email ?message ?(news = Change.make ()) changes =
    let head = unsafe_head
    and read = unsafe_read
    and is_empty = unsafe_is_empty
    and tree_root = unsafe_tree_root in
    if head t = None && not (is_empty t)
    then Error (`Not_found (branch t))
    else
    let* tree = Change.root ~read:(read t) ~news (tree_root t) changes in
    let user = author_of ~now:t.now ~author ~email in
    let parents = Option.to_list (unsafe_head t) in
    let message = Option.value ~default:"Committed by mgit" message in
    let message = Some (message ^ "\n") in
    let commit = Mgit_object.Commit.make ~tree ~parents ~author:user ~committer:user message in
    let root = Change.add news `A (Mgit_object.Commit.to_string commit) in
    generation t ~news (replace (unsafe_references t) (branch t) root)

  let pack_of t ?(lookup = Fun.const None) uids =
    let entries = entries t ~lookup (Change.make ()) uids in
    let load = loader t in
    let targets = Delta.delta ~load (List.to_seq entries) in
    Pack.to_seq ~load ~number_of_objects:(List.length entries) targets

  let push_branches = push (* NOTE(dinosaure): [Sync.push], before we shadow it. *)

  let push t =
    let store = { Sync.references= unsafe_references; read= unsafe_read; branch } in
    let pack uids = Ok (pack_of t uids) in
    push_branches t store ~branches:[ branch t ] ~pack t.remote

  (** Changes *)

  (* {2 Exclusion.}

     Whatever publishes a new generation holds the lock of the repository: a
     change (or a [change_and_push]) and a [pull] or a [gc] which would happen
     "at the same time" (in another task) wait for their turn. The handle given
     to the function of a [change_and_push] only stages changes: from it,
     publishing is refused rather than waiting for a lock its own transaction
     holds. The same goes for the task which holds the lock and uses another
     handle of the repository: [Miou.Mutex] tells us it already owns it. *)

  let exclusively t fn =
    match t.staged with
    | Some _ -> error_msgf "Not allowed inside a change_and_push"
    | None ->
        let entered = ref false in
        begin match
          Miou.Mutex.protect t.shared.lock @@ fun () ->
          entered := true;
          fn ()
        with
        | value -> value
        | exception Sys_error _ when not !entered ->
            error_msgf "Not allowed inside a change_and_push"
        end

  let rec add staged change =
    match Atomic.get staged.changes with
    | Closed -> error_msgf "The change_and_push is over"
    | Open changes as seen ->
        if Atomic.compare_and_set staged.changes seen (Open (change :: changes))
        then Ok ()
        else add staged change

  let stage t change =
    match t.staged with
    | Some staged -> add staged change
    | None ->
        exclusively t @@ fun () ->
        let* () = apply t [ change ] in
        push t

  let set t ?(perm = `Normal) path contents =
    stage t (segments path, `Set ((perm :> Mgit_object.Tree.perm), contents))

  let remove t path = stage t (segments path, `Rem)

  let change_and_push t ?author ?email ?message fn =
    match t.staged with
    | Some _ -> error_msgf "Nested change_and_push"
    | None ->
        exclusively t @@ fun () ->
        let staged = { news= Change.make (); changes= Atomic.make (Open []) } in
        let close () = Atomic.exchange staged.changes Closed in
        let value =
          match fn { t with staged= Some staged } with
          | value -> value
          | exception exn ->
              let bt = Printexc.get_raw_backtrace () in
              ignore (close ());
              Printexc.raise_with_backtrace exn bt in
        begin match close () with
        | Closed | Open [] -> Ok value
        | Open changes ->
            let changes = List.rev changes in
            let* () = apply t ?author ?email ?message ~news:staged.news changes in
            let* () = push t in
            Ok value
        end

  let of_received (received : _ received) =
    let carton, cursor = received.meta in
    { kind= received.kind; length= received.length; meta= Tmp (carton, cursor) }

  let infinite_depth = 0x7fffffff

  let unsafe_pull : type k. ?only:string list -> k t
      -> ((string * change list) list, [> error ]) result =
   fun ?only t ->
    let references = unsafe_references
    and read = unsafe_read in
    let store = { Sync.references; read; branch } in
    let generation t ~references ~lookup ~reader ~news =
      let empty = unsafe_is_empty t in
      let lookup uid = Option.map of_received (lookup uid) in
      let* () = generation t ~lookup ~reader ~news references in
      (* an empty repository follows the [HEAD] of the remote *)
      begin match references with
      | (name, _) :: _ when empty && not (List.mem_assoc (branch t) references) ->
          Atomic.set t.branch name
      | _ -> () end;
      Ok () in
    let unshallow = t.shared.depth <= 0 && shallows t store <> [] in
    let deepen =
      if t.shared.depth > 0 then Some t.shared.depth
      else if unshallow then Some infinite_depth
      else None in
    match store_of t with
    | Blk_store tmp ->
        let tmp =
          { seq= (fun ~len -> Store.Tmp.seq tmp ~len)
          ; carton= (fun ~len ~extern -> Store.Tmp.carton ~extern tmp ~len)
          ; into= Store.Tmp.sink tmp } in
        begin try pull t store ~generation ?deepen ~unshallow ?only tmp t.remote
        with Blk.Append.Out_of_space -> Error `Out_of_space end
    | Mem_store tmp ->
        let tmp = { seq= (fun ~len -> Mgit_mem.Tmp.seq tmp ~len)
                  ; carton= (fun ~len ~extern -> Mgit_mem.Tmp.carton ~extern tmp ~len)
                  ; into= Mgit_mem.Tmp.sink tmp } in
        pull t store ~generation ?deepen ~unshallow ?only tmp t.remote

  let pull t = exclusively t @@ fun () -> unsafe_pull t

  type update = { name : string; old : uid option; uid : uid }

  let write (Flux.Sink into) seq =
    let acc = ref (into.init ()) and len = ref 0 in
    let fn str =
      len := !len + String.length str;
      acc := into.push !acc str in
    Seq.iter fn seq;
    into.stop !acc;
    !len

  let load : type k. k t -> updates:update list -> string Seq.t
      -> ((string * change list) list, [> error ]) result =
   fun t ~updates pack ->
    exclusively t @@ fun () ->
    let updates = List.map (fun u -> { u with name= reference u.name }) updates in
    let current = unsafe_references t in
    let pp_uid = Fmt.option ~none:(Fmt.any "nothing") Mgit_object.pp_uid in
    let* () =
      let fn acc { name; old; _ } =
        let* () = acc in
        let actual = List.assoc_opt name current in
        if Option.equal Carton.Uid.equal actual old then Ok ()
        else error_msgf "%s is at %a, not at %a" name pp_uid actual pp_uid old in
      List.fold_left fn (Ok ()) updates in
    let store = { Sync.references= unsafe_references; read= unsafe_read; branch } in
    let go tmp =
      let len = write tmp.into pack in
      let* lookup, reader = ingest t store tmp ~len in
      let lookup uid = Option.map of_received (lookup uid) in
      let* () =
        let fn acc { name; uid; _ } =
          let* () = acc in
          match reader uid with
          | Some (`A, _) -> Ok ()
          | Some _ -> error_msgf "%s: %a is not a commit" name Mgit_object.pp_uid uid
          | None -> error_msgf "%s: %a is unavailable" name Mgit_object.pp_uid uid in
        List.fold_left fn (Ok ()) updates in
      let targets =
        List.fold_left (fun refs { name; uid; _ } -> replace refs name uid) current updates in
      let* () =
        let store =
          { Sync.references= (fun _ -> targets)
          ; read= (fun _ uid -> reader uid)
          ; branch= (fun _ -> branch t) } in
        let pack uids = Ok (pack_of t ~lookup uids) in
        let branches = List.map (fun { name; _ } -> name) updates in
        push_branches t store ~branches ~pack t.remote in
      let before =
        List.map (fun { name; _ } -> (name, paths t store (List.assoc_opt name current))) updates in
      let* () = generation t ~lookup ~reader ~news:(Change.make ()) targets in
      let afters = unsafe_references t in
      let fn (name, before) =
        match diff ~before ~after:(paths t store (List.assoc_opt name afters)) with
        | [] -> None
        | changes -> Some (name, changes) in
      Ok (List.filter_map fn before) in
    match store_of t with
    | Blk_store tmp ->
        let tmp =
          { seq= (fun ~len -> Store.Tmp.seq tmp ~len)
          ; carton= (fun ~len ~extern -> Store.Tmp.carton ~extern tmp ~len)
          ; into= Store.Tmp.sink tmp } in
        begin try go tmp with Blk.Append.Out_of_space -> Error `Out_of_space end
    | Mem_store tmp ->
        let tmp =
          { seq= (fun ~len -> Mgit_mem.Tmp.seq tmp ~len)
          ; carton= (fun ~len ~extern -> Mgit_mem.Tmp.carton ~extern tmp ~len)
          ; into= Mgit_mem.Tmp.sink tmp } in
        go tmp

  (* Branches *)

  let checkout t name =
    let name = reference name in
    exclusively t @@ fun () ->
    let handle = { t with branch= Atomic.make name; pinned= None } in
    let locally () =
      match unsafe_head t with
      | None -> Ok handle
      | Some uid ->
          let refs = unsafe_references t @ [ (name, uid) ] in
          let* () = generation t ~news:(Change.make ()) refs in
          Ok handle in
    if List.mem_assoc name (unsafe_references t)
    then Ok handle
    else
      match t.remote with
      | None -> locally ()
      | Some _ ->
          begin match unsafe_pull ~only:[ name ] t with
          | Ok _ -> Ok handle
          | Error (`Not_found name') when name = name' -> locally ()
          | Error _ as err -> err
          end

  let forget t name =
    let name = reference name in
    exclusively t @@ fun () ->
    let references = unsafe_references t in
    if not (List.mem_assoc name references)
    then Error (`Not_found name)
    else
      match List.remove_assoc name references with
      | [] -> error_msgf "%s is the last branch of the repository" name
      | references -> generation t ~news:(Change.make ()) references

  let set_default t name =
    let name = reference name in
    exclusively t @@ fun () ->
    let references = unsafe_references t in
    match List.assoc_opt name references with
    | None -> Error (`Not_found name)
    | Some _ when fst (List.hd references) = name -> Ok ()
    | Some uid ->
        generation t ~news:(Change.make ())
          ((name, uid) :: List.remove_assoc name references)

  let gc ?shallows t =
    exclusively t @@ fun () ->
    let depth = Option.value ~default:t.shared.depth shallows in
    match unsafe_references t with
    | [] -> Ok ()
    | references ->
        t.shared.depth <- depth;
        generation t ~depth ~news:(Change.make ()) references

  let rank = function `A -> 0 | `B -> 1 | `C -> 2 | `D -> 3

  let unsafe_to_bundle : type k. k t -> string Seq.t =
   fun t ->
    match store_of t with
    | Blk_store store -> Store.to_bundle store
    | Mem_store store when Mgit_mem.is_empty store -> Seq.empty
    | Mem_store store ->
        let uids = Mgit_mem.uids store in
        let key uid = rank (Option.get (Mgit_mem.kind store uid)) in
        let uids = List.stable_sort (fun a b -> Int.compare (key a) (key b)) uids in
        let entries = entries t ~lookup:(Fun.const None) (Change.make ()) uids in
        let load = loader t in
        let targets = Delta.delta ~load (List.to_seq entries) in
        let number_of_objects = List.length entries in
        let pack = Pack.to_seq ~load ~number_of_objects targets in
        let hdr =
          Bundle.make ~ref_length:Mgit_object.ref_length
            ~prerequisites:(Mgit_mem.prerequisites store)
            (Mgit_mem.references store) in
        Seq.cons (Bundle.to_string hdr) pack

  let protect : type k a. k store -> (unit -> a) -> a =
   fun store fn ->
    match store with
    | Blk_store store -> Store.protect store fn
    | Mem_store _ -> fn ()

  let rec reading : type k a. k t -> (k t -> a) -> a =
   fun t fn ->
    match t.pinned with
    | Some _ -> fn t
    | None ->
        let store = Atomic.get t.shared.store in
        let result =
          protect store @@ fun () ->
          if Atomic.get t.shared.store != store then None
          else Some (fn { t with pinned= Some store }) in
        match result with Some value -> value | None -> reading t fn

  let get t path = reading t @@ fun t -> unsafe_get t path
  let list t path = reading t @@ fun t -> unsafe_list t path
  let fold t fn acc = reading t @@ fun t -> unsafe_fold t fn acc
  let commit t = reading t unsafe_commit
  let branches t = reading t unsafe_references
  let mem t uid = reading t @@ fun t -> Option.is_some (unsafe_read t uid)

  let shallows t = reading t @@ fun t ->
    let references = unsafe_references
    and read = unsafe_read in
    shallows t { Sync.references; read; branch }

  let to_pack t ?(haves = []) wants fn =
    reading t @@ fun t ->
    let* () =
      let check acc uid =
        let* () = acc in
        match unsafe_read t uid with
        | Some (`A, _) -> Ok ()
        | Some _ -> error_msgf "%a is not a commit" Mgit_object.pp_uid uid
        | None -> error_msgf "%a is unavailable" Mgit_object.pp_uid uid in
      List.fold_left check (Ok ()) wants in
    let read = unsafe_read t in
    let* uids = Closure.uncommon ~read ~exclude:haves wants in
    Seq.iter fn (pack_of t uids);
    Ok ()

  let to_bundle t fn = reading t @@ fun t -> Seq.iter fn (unsafe_to_bundle t)

  let push t ?branches remote =
    reading t @@ fun t ->
    let branches =
      match branches with
      | Some branches -> List.map reference branches
      | None -> List.map fst (unsafe_references t) in
    let store = { Sync.references= unsafe_references; read= unsafe_read; branch } in
    let pack uids = Ok (pack_of t uids) in
    push_branches t store ~branches ~pack (Some remote)
end
