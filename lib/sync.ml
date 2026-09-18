let src = Logs.Src.create "mgit.sync"

module Log = (val Logs.src_log src : Logs.LOG)

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let ( let* ) = Result.bind

type uid = Carton.Uid.t

type error =
  [ `Msg of string
  | `Zone_full
  | `Not_found of string ]

type 't store =
  { head : 't -> uid option
  ; tree_root : 't -> uid option
  ; read : 't -> uid -> ([ `A | `B | `C | `D ] * string) option
  ; branch : 't -> string
  ; is_empty : 't -> bool }

module Make (Flow : Git_flow.S) = struct
  module Run = Git_flow.Make (Flow)

  type remote = { ctx : Flow.ctx; edn : Endpoint.t; version : [ `V1 | `V2 ] }

  let reword t = Protocol.reword_error (fun err -> `Msg (Fmt.str "%a" Smart.pp_error err)) t

  let paths t store =
    let tbl = Hashtbl.create 0x100 in
    let rec go rev_path uid =
      match store.read t uid with
      | Some (`B, payload) ->
          begin match Git_object.Tree.of_string payload with
          | Error (`Msg _) -> ()
          | Ok tree ->
              let each { Git_object.Tree.perm; name; node } =
                match perm with
                | `Dir -> go (name :: rev_path) node
                | _ ->
                    let path = "/" ^ String.concat "/" (List.rev (name :: rev_path)) in
                    Hashtbl.replace tbl path node in
              List.iter each (Git_object.Tree.to_list tree)
          end
      | _ -> () in
    begin match store.tree_root t with
    | Some root -> go [] root | None -> () end;
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

  let commits t store =
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
              go (rest @ Git_object.parents_of_commit payload)
          | _ -> go rest
          end in
    begin match store.head t with Some uid -> go [ uid ] | None -> () end;
    List.rev !acc

  let shallows t store =
    let fn uid =
      match store.read t uid with
      | Some (`A, payload) ->
          let parents = Git_object.parents_of_commit payload in
          List.exists (fun uid -> store.read t uid = None) parents
      | _ -> false in
    List.filter fn (commits t store)

  let negotiator t store ~deepen refs =
    let load uid =
      match store.read t uid with
      | Some (`A, payload) ->
          begin match Git_object.Commit.of_string payload with
          | Ok { Git_object.Commit.committer= { date= (date, _); _ }; parents; _ } ->
              Some (date, parents)
          | Error _ -> None
          end
      | _ -> None in
    let negotiator = Negotiator.make ~load in
    if deepen = None then begin
      let fn (_, hex) =
        match Git_object.uid_of_hex hex with
        | Ok uid when load uid <> None -> Negotiator.known_common negotiator uid
        | _ -> () in
      List.iter fn refs.Smart.refs
    end;
    Option.iter (Negotiator.add_tip negotiator) (store.head t);
    negotiator

  let want t store refs =
    match List.assoc_opt (store.branch t) refs.Smart.refs with
    | Some hex -> Ok (store.branch t, Git_object.uid_of_hex_exn hex)
    | None ->
        begin match (store.is_empty t, refs.Smart.head_symref) with
        | true, Some name ->
            begin match List.assoc_opt name refs.Smart.refs with
            | Some hex -> Ok (name, Git_object.uid_of_hex_exn hex)
            | None -> error_msgf "%s does not exist on the remote" name
            end
        | _ -> error_msgf "%s does not exist on the remote" (store.branch t)
        end

  let receive t store ?deepen flow ctx advertisement refs want into =
     let q = Flux.Bqueue.(create with_close) 0x100 in
     let received = ref 0 in
     let consumer =
       Miou.async @@ fun () ->
       let from = Flux.Source.bqueue q in
       let via = Flux.Flow.tap (fun str -> received := !received + String.length str) in
       let (), leftover = Flux.Stream.run ~from ~via ~into in
       Option.iter Flux.Source.dispose leftover in
     let shallows = shallows t store in
     (* let deepen = if t.depth > 0 then Some t.depth else None in *)
     let negotiator = negotiator t store ~deepen refs in
     let fetch =
       match advertisement with
       | Smart.V2 { capabilities } ->
           Find_common.fetch_v2 ~thin:true ~capabilities ~negotiator ~shallows
             ?deepen [ want ] q ctx
       | Smart.V1 { capabilities; _ } ->
           Find_common.fetch_v1 ~thin:true ~capabilities ~negotiator ~shallows
             ?deepen [ want ] q ctx in
     let result = Run.run flow (reword fetch) in
     Flux.Bqueue.close q;
     Miou.await_exn consumer;
     begin match result with
     | Error (`Msg _) as err -> err
     | Ok (_, true) ->
         error_msgf "The remote reported an error during the fetch"
     | Ok (_shallow_info, false) ->
         Log.debug (fun m -> m "PACK of %d byte(s) received" !received);
         Ok !received
     end

  type 'tmp located = { kind : Carton.Kind.t; length : int; meta : 'tmp Carton.t * int }

  let extern t store =
    let cache = Hashtbl.create 0x10 in
    fun (uid : uid) ->
      match Hashtbl.find_opt cache (uid :> string) with
      | Some value -> value
      | None ->
          let fn (kind, payload) = (kind, Bstr.of_string payload) in
          let value = Option.map fn (store.read t uid) in
          Hashtbl.replace cache (uid :> string) value;
          value

  let analyse ~extern seq carton =
     let src = Flux.Source.seq seq in
     let via = Carton_miou_flux.first_pass
       ~digest:(Git_object.digest_pack ())
       ~ref_length:Git_object.ref_length in
     let into = Carton_miou_flux.oracle ~identify:Git_object.identify in 
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
     let src = Carton_miou_flux.entries ~threads:0 ~extern carton oracle in
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
    -> branch:string
    -> lookup:(uid -> 'tmp located option)
    -> reader:Change.read
    -> news:Change.news
    -> uid
    -> (unit, 'err) result

  type 'tmp tmp =
    { seq : len:int -> string Seq.t
    ; carton :
        len:int -> extern:(uid -> (Carton.Kind.t * Bstr.t) option) -> 'tmp Carton.t
    ; into : (string, unit) Flux.sink }

  let pull t store ~generation ?deepen tmp = function
    | None -> error_msgf "No remote configured"
    | Some { ctx= remote_ctx; edn; version } ->
        let* flow = Flow.connect remote_ctx edn in
        let finally () = Flow.close flow in
        Fun.protect ~finally @@ fun () ->
        let ctx = Protocol.ctx () in
        let host = edn.Endpoint.host and path = edn.Endpoint.path in
        let request =
          let version = match version with `V1 -> 1 | `V2 -> 2 in
          Smart.proto_request ~version ~service:"git-upload-pack" ~host path ctx in
        let* () = Run.run flow (reword request) in
        let* advertisement = Run.run flow (reword (Smart.advertisement ctx)) in
        let* refs =
          match advertisement with
          | Smart.V1 { refs; _ } -> Ok refs
          | Smart.V2 _ -> Run.run flow (reword (Smart.ls_refs ctx)) in
        let* name, want = want t store refs in
        if Some want = store.head t then Ok []
        else begin
          let before = paths t store in
          (* NOTE(dinosaure): download the PACK file into [tmp]. *)
          let* len = receive t store ?deepen flow ctx advertisement refs want tmp.into in
          if len = 0 then Ok []
          else begin
            (* NOTE(dinosaure): analyze the PACK file. *)
            let extern = extern t store in
            let carton = tmp.carton ~len ~extern in
            let* _carton, tbl = analyse ~extern (tmp.seq ~len) carton in
            let lookup uid = Hashtbl.find_opt tbl (uid : uid :> string) in
            let reader uid =
              match lookup uid with
              | None -> store.read t uid
              | Some { meta= carton, cursor; _ } ->
                  let value = value_of_cursor carton ~cursor in
                  let len = Carton.Value.length value in
                  let bstr = Carton.Value.bigstring value in
                  Some (Carton.Value.kind value, Bstr.sub_string bstr ~off:0 ~len)
            in
            let news = Change.make () in
            (* NOTE(dinosaure): save it. *)
            let* () = generation t ~branch:name ~lookup ~reader ~news want in
            let after = paths t store in
            Ok (diff ~before ~after)
          end
        end
end
