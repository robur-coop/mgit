let src = Logs.Src.create "mgit.sync"

module Log = (val Logs.src_log src : Logs.LOG)

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let ( let* ) = Result.bind

type uid = Carton.Uid.t

type error =
  [ `Msg of string
  | `Out_of_space
  | `Not_found of string ]

type 't store =
  { references : 't -> (string * uid) list
  ; read : 't -> uid -> ([ `A | `B | `C | `D ] * string) option
  ; branch : 't -> string }

let head t store = List.assoc_opt (store.branch t) (store.references t)

module Endpoint = Endpoint

type data = [ `End | `Len of int ]

module type S = sig
  type ctx
  type t

  val connect :
       ctx
    -> Endpoint.t
    -> service:string
    -> version:int
    -> (t, [> `Msg of string ]) result

  val recv : t -> bytes -> off:int -> len:int -> (data, [> `Msg of string ]) result
  val send : t -> string -> off:int -> len:int -> (int, [> `Msg of string ]) result
  val close : t -> unit
end

module Make (Flow : S) = struct
  let run flow t =
    let rec go = function
      | Protocol.Return value -> Ok value
      | Protocol.Error err -> Error err
      | Protocol.Read { buffer; off; len; k } ->
          begin match Flow.recv flow buffer ~off ~len with
          | Ok value -> go (k value)
          | Error (`Msg _ as err) -> Error err
          | Error _ as err -> err
          end
      | Protocol.Write { buffer; off; len; k } ->
          begin match Flow.send flow buffer ~off ~len with
          | Ok len -> go (k len)
          | Error (`Msg _ as err) -> Error err
          | Error _ as err -> err
          end in
    go t

  type remote = { ctx : Flow.ctx; edn : Endpoint.t }

  let reword t = Protocol.reword_error (fun err -> `Msg (Fmt.str "%a" Smart.pp_error err)) t

  let paths t store commit =
    let tbl = Hashtbl.create 0x100 in
    let rec go rev_path uid =
      match store.read t uid with
      | Some (`B, payload) ->
          begin match Mgit_object.Tree.of_string payload with
          | Error (`Msg _) -> ()
          | Ok tree ->
              let each { Mgit_object.Tree.perm; name; node } =
                match perm with
                | `Dir -> go (name :: rev_path) node
                | _ ->
                    let path = "/" ^ String.concat "/" (List.rev (name :: rev_path)) in
                    Hashtbl.replace tbl path node in
              List.iter each (Mgit_object.Tree.to_list tree)
          end
      | _ -> () in
    let tree_root =
      match Option.map (store.read t) commit with
      | Some (Some (`A, payload)) -> Mgit_object.tree_of_commit payload
      | _ -> None in
    Option.iter (go []) tree_root;
    tbl

  let diff ~before ~after =
    let changes = ref [] in
    let removed_or_changed path uid =
      match Hashtbl.find_opt after path with
      | None -> changes := `Rem path :: !changes
      | Some uid' when not (Carton.Uid.equal uid uid') ->
          changes := `Set path :: !changes
      | Some _ -> () in
    let added path _uid =
      if not (Hashtbl.mem before path) then changes := `Add path :: !changes in
    Hashtbl.iter removed_or_changed before;
    Hashtbl.iter added after;
    List.sort_uniq compare !changes

  let commits t store roots =
    let seen = Hashtbl.create 0x10 in
    let acc = ref [] in
    let rec go = function
      | [] -> ()
      | uid :: rest when Hashtbl.mem seen (uid : uid :> string) -> go rest
      | uid :: rest ->
          Hashtbl.replace seen (uid : uid :> string) ();
          begin match store.read t uid with
          | Some (`A, payload) ->
              acc := uid :: !acc;
              go (rest @ Mgit_object.parents_of_commit payload)
          | _ -> go rest
          end in
    go roots;
    List.rev !acc

  let heads t store = List.map snd (store.references t)

  let is_ancestor t store ~of_:root uid =
    let fn commit =
      Carton.Uid.equal commit uid
      ||
      match store.read t commit with
      | Some (`A, payload) ->
          List.exists (Carton.Uid.equal uid) (Mgit_object.parents_of_commit payload)
      | _ -> false in
    List.exists fn (commits t store [ root ])

  let shallows t store =
    let fn uid =
      match store.read t uid with
      | Some (`A, payload) ->
          let parents = Mgit_object.parents_of_commit payload in
          List.exists (fun uid -> store.read t uid = None) parents
      | _ -> false in
    List.filter fn (commits t store (heads t store))

  let negotiator t store ~deepen refs =
    let load uid =
      match store.read t uid with
      | Some (`A, payload) ->
          begin match Mgit_object.Commit.of_string payload with
          | Ok { Mgit_object.Commit.committer= { date= (date, _); _ }; parents; _ } ->
              Some (date, parents)
          | Error _ -> None
          end
      | _ -> None in
    let negotiator = Negotiator.make ~load in
    if deepen = None then begin
      let fn (_, hex) =
        match Mgit_object.uid_of_hex hex with
        | Ok uid when load uid <> None -> Negotiator.known_common negotiator uid
        | _ -> () in
      List.iter fn refs.Smart.refs
    end;
    List.iter (Negotiator.add_tip negotiator) (heads t store);
    negotiator

  let replace references name uid =
    if List.mem_assoc name references
    then List.map (fun (name', uid') -> if name = name' then (name, uid) else (name', uid')) references
    else references @ [ (name, uid) ]

  let targets t store ?only refs =
    let remote name =
      Option.map Mgit_object.uid_of_hex_exn (List.assoc_opt name refs.Smart.refs) in
    let locals = store.references t in
    match only with
    | Some names ->
        let fn acc name =
          let* acc = acc in
          match remote name with
          | Some uid -> Ok (replace acc name uid)
          | None -> Error (`Not_found name) in
        List.fold_left fn (Ok locals) names
    | None when locals = [] ->
        let name = store.branch t in
        begin match (remote name, refs.Smart.head_symref) with
        | Some uid, _ -> Ok [ (name, uid) ]
        | None, Some name' when Option.is_some (remote name') ->
            Ok [ (name', Option.get (remote name')) ]
        | None, _ -> error_msgf "%s does not exist on the remote" name
        end
    | None ->
        let fn (name, uid) = (name, Option.value ~default:uid (remote name)) in
        Ok (List.map fn locals)

  let receive t store ?deepen ~stateless flow ctx advertisement refs wants
      (Flux.Sink into) =
    let acc = ref (into.init ()) and received = ref 0 in
    let push str =
      received := !received + String.length str;
      acc := into.push !acc str in
    let shallows = shallows t store in
    let negotiator = negotiator t store ~deepen refs in
    let fetch =
      match advertisement with
      | Smart.V2 { capabilities } ->
          Find_common.fetch_v2 ~thin:true ~capabilities ~negotiator ~shallows
            ?deepen wants push ctx
      | Smart.V1 { capabilities; _ } ->
          Find_common.fetch_v1 ~stateless ~thin:true ~capabilities ~negotiator
            ~shallows ?deepen wants push ctx in
    match run flow (reword fetch) with
    | Error (`Msg _) as err -> err
    | Ok (_, true) -> error_msgf "The remote reported an error during the fetch"
    | Ok (_shallow_info, false) ->
        into.stop !acc;
        Log.debug (fun m -> m "PACK of %d byte(s) received" !received);
        Ok !received

  type 'tmp located = { kind : Carton.Kind.t; length : int; meta : 'tmp Carton.t * int }

  let extern t store =
    let cache = Hashtbl.create 0x10 and mutex = Mutex.create () in
    fun (uid : uid) ->
      match Mutex.protect mutex (fun () -> Hashtbl.find_opt cache (uid :> string)) with
      | Some value -> value
      | None ->
          let fn (kind, payload) = (kind, Bstr.of_string payload) in
          let value = Option.map fn (store.read t uid) in
          Mutex.protect mutex (fun () -> Hashtbl.replace cache (uid :> string) value);
          value

  let analyse ~extern seq carton =
     let src = Flux.Source.seq seq in
     let via = Carton_miou_flux.first_pass
       ~digest:(Mgit_object.digest_pack ())
       ~ref_length:Mgit_object.ref_length in
     let into = Carton_miou_flux.oracle ~identify:Mgit_object.identify in 
     let oracle =
       Flux.Stream.from src 
       |> Flux.Stream.via via
       |> Flux.Stream.into into in
     let tbl = Hashtbl.create 0x7ff in
     let fn (value, cursor, uid) =
       let located =
         { kind= Carton.Value.kind value
         ; length= Carton.Value.length value
         ; meta= carton, cursor } in
       Hashtbl.replace tbl (uid : Carton.Uid.t :> string) located in
     let threads = Int.min 4 (Miou.Domain.available ()) in
     let src = Carton_miou_flux.entries ~threads ~extern carton oracle in
     match Flux.Source.each fn src with
     | exception Failure msg -> Error (`Msg msg)
     | () ->
         if Hashtbl.length tbl <> oracle.Carton.number_of_objects
         then error_msgf "The received PACK file refers to objects we do not have"
         else Ok (carton, tbl)

  let value_of_cursor carton ~cursor =
    let size = Carton.size_of_offset carton ~cursor Carton.Size.zero in
    let blob = Carton.Blob.make ~size in
    Carton.of_offset carton blob ~cursor

  type ('t, 'tmp, 'err) generation =
       't
    -> references:(string * uid) list
    -> lookup:(uid -> 'tmp located option)
    -> reader:Mgit_change.read
    -> news:Mgit_change.news
    -> (unit, 'err) result

  type 'tmp tmp =
    { seq : len:int -> string Seq.t
    ; carton :
        len:int -> extern:(uid -> (Carton.Kind.t * Bstr.t) option) -> 'tmp Carton.t
    ; into : (string, unit) Flux.sink }

  let ingest t store tmp ~len =
    let* lookup =
      if len = 0 then Ok (Fun.const None)
      else
        let extern = extern t store in
        let carton = tmp.carton ~len ~extern in
        let* _carton, tbl = analyse ~extern (tmp.seq ~len) carton in
        Ok (fun (uid : uid) -> Hashtbl.find_opt tbl (uid :> string)) in
    let reader uid =
      match lookup uid with
      | None -> store.read t uid
      | Some { meta= carton, cursor; _ } ->
          let value = value_of_cursor carton ~cursor in
          let len = Carton.Value.length value in
          let bstr = Carton.Value.bigstring value in
          Some (Carton.Value.kind value, Bstr.sub_string bstr ~off:0 ~len) in
    Ok (lookup, reader)

  let is_stateless edn =
    match edn.Endpoint.scheme with `HTTP | `HTTPS -> true | `Git | `SSH -> false

  (* NOTE(dinosaure): here, we can not use [Miou.Ownership] because [Mgit_http]
     has a [close] which emits effects. So we can not create a resource with a
     [finally] which closes. *)

  let reraise exn = Printexc.raise_with_backtrace exn (Printexc.get_raw_backtrace ())

  let with_flow flow fn =
    match fn () with
    | value -> Flow.close flow; value
    | exception (Miou.Cancelled as exn) -> reraise exn
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        Flow.close flow;
        Printexc.raise_with_backtrace exn bt

  let connect remote_ctx edn ~service ~version =
    let* flow = Flow.connect remote_ctx edn ~service ~version in
    let ctx = Protocol.ctx () in
    match edn.Endpoint.scheme with
    | `SSH | `HTTP | `HTTPS -> Ok (flow, ctx)
    | `Git ->
        let host = edn.Endpoint.host and path = edn.Endpoint.path in
        let request = Smart.proto_request ~version ~service ~host path ctx in
        begin match run flow (reword request) with
        | Ok () -> Ok (flow, ctx)
        | Error _ as err -> Flow.close flow; err
        | exception (Miou.Cancelled as exn) -> reraise exn
        | exception exn ->
            let bt = Printexc.get_raw_backtrace () in
            Flow.close flow;
            Printexc.raise_with_backtrace exn bt
        end

  let pull t store ~generation ?deepen ?only tmp = function
    | None -> error_msgf "No remote configured"
    | Some { ctx= remote_ctx; edn } ->
        let* flow, ctx = connect remote_ctx edn ~service:"git-upload-pack" ~version:2 in
        with_flow flow @@ fun () ->
        let* advertisement = run flow (reword (Smart.advertisement ctx)) in
        let* refs =
          match advertisement with
          | Smart.V1 { refs; _ } -> Ok refs
          | Smart.V2 _ -> run flow (reword (Smart.ls_refs ctx)) in
        let stateless = is_stateless edn in
        let done_ () =
          if not stateless
          then ignore (run flow (reword (Protocol.encode_flush_pkt ctx))) in
        let locals = store.references t in
        let* targets = targets t store ?only refs in
        if targets = locals then begin done_ (); Ok [] end
        else begin
          let before =
            let fn (name, _) = (name, paths t store (List.assoc_opt name locals)) in
            List.map fn targets in
          (* NOTE(dinosaure): a commit we already have (another branch) is not
             asked for. *)
          let wants =
            let fn acc (_, uid) =
              if store.read t uid = None && not (List.mem uid acc)
              then uid :: acc else acc in
            List.rev (List.fold_left fn [] targets) in
          let* received =
            if wants = [] then begin done_ (); Ok None end
            else
              (* NOTE(dinosaure): download the PACK file into [tmp]. *)
              let* len =
                receive t store ?deepen ~stateless flow ctx advertisement refs wants
                  tmp.into in
              if len = 0 then Ok None
              else
                (* NOTE(dinosaure): analyze the PACK file. *)
                let* lookup, reader = ingest t store tmp ~len in
                Ok (Some (lookup, reader)) in
          let lookup, reader =
            match received with
            | None -> (Fun.const None, store.read t)
            | Some (lookup, reader) -> (lookup, reader) in
          if wants <> [] && received = None then Ok []
          else begin
            let news = Mgit_change.make () in
            (* NOTE(dinosaure): save it. *)
            let* () = generation t ~references:targets ~lookup ~reader ~news in
            let afters = store.references t in
            let fn (name, before) =
              let after = paths t store (List.assoc_opt name afters) in
              match diff ~before ~after with
              | [] -> None
              | changes -> Some (name, changes) in
            Ok (List.filter_map fn before)
          end
        end

  let push t store ~branches ~pack = function
    | None -> Ok ()
    | Some { ctx= remote_ctx; edn; _ } ->
        let locals = store.references t in
        let fn name = Option.map (fun uid -> (name, uid)) (List.assoc_opt name locals) in
        let branches = List.filter_map fn branches in
        if branches = [] then Ok ()
        else
          let* flow, ctx = connect remote_ctx edn ~service:"git-receive-pack" ~version:1 in
          with_flow flow @@ fun () ->
          let* advertisement = run flow (reword (Smart.advertisement ctx)) in
          match advertisement with
          | Smart.V2 _ -> error_msgf "Unexpected protocol v2 from git-receive-pack"
          | Smart.V1 { refs; capabilities } ->
              let command (name, new_uid) =
                let old_uid =
                  Option.map Mgit_object.uid_of_hex_exn (List.assoc_opt name refs.Smart.refs) in
                match old_uid with
                | Some old_uid when Carton.Uid.equal old_uid new_uid -> Ok None
                | Some old_uid when not (is_ancestor t store ~of_:new_uid old_uid) ->
                    error_msgf "%s: the remote has commits we do not have \
                                (non-fast-forward), pull first" name
                | old_uid -> Ok (Some { Smart.old_uid; new_uid; name }) in
              let* commands =
                List.fold_left
                  (fun acc branch ->
                    let* acc = acc in
                    let* command = command branch in
                    Ok (Option.fold ~none:acc ~some:(fun c -> c :: acc) command))
                  (Ok []) branches in
              let commands = List.rev commands in
              if commands = [] then Ok ()
              else
                let exclude =
                  let fn (_, hex) =
                    match Mgit_object.uid_of_hex hex with
                    | Ok uid when Option.is_some (store.read t uid) -> Some uid
                    | _ -> None in
                  List.filter_map fn refs.Smart.refs in
                let roots = List.map (fun { Smart.new_uid; _ } -> new_uid) commands in
                let* uids = Mgit_closure.uncommon ~read:(store.read t) ~exclude roots in
                Log.debug (fun m -> m "push %d object(s)" (List.length uids));
                let* seq = pack uids in
                let capabilities =
                  List.filter
                    (fun cap -> List.mem cap capabilities)
                    [ "report-status"; "ofs-delta"; "atomic" ] in
                let exchange =
                  let ( let* ) = Protocol.bind in
                  let* () = Smart.send_commands ~capabilities commands ctx in
                  let* () = Smart.send_seq seq ctx in
                  if List.mem "report-status" capabilities
                  then
                    let* report = Smart.report_status ctx in
                    Protocol.return (Some report)
                  else Protocol.return None in
                let* report = run flow (reword exchange) in
                match report with
                | None -> Ok ()
                | Some { Smart.unpack= Error reason; _ } ->
                    error_msgf "The remote refused our PACK file: %s" reason
                | Some { Smart.unpack= Ok (); statuses } ->
                    let fn acc { Smart.name; _ } =
                      let* () = acc in
                      match List.assoc_opt name statuses with
                      | Some (Error reason) ->
                          error_msgf "The remote refused to update %s: %s" name reason
                      | Some (Ok ()) | None -> Ok () in
                    List.fold_left fn (Ok ()) commands
end
