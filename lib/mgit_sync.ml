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
  { head : 't -> uid option
  ; tree_root : 't -> uid option
  ; read : 't -> uid -> ([ `A | `B | `C | `D ] * string) option
  ; branch : 't -> string
  ; is_empty : 't -> bool }

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

  let paths t store =
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
              go (rest @ Mgit_object.parents_of_commit payload)
          | _ -> go rest
          end in
    begin match store.head t with Some uid -> go [ uid ] | None -> () end;
    List.rev !acc

  let is_ancestor t store uid =
    let fn commit =
      Carton.Uid.equal commit uid
      ||
      match store.read t commit with
      | Some (`A, payload) ->
          List.exists (Carton.Uid.equal uid) (Mgit_object.parents_of_commit payload)
      | _ -> false in
    List.exists fn (commits t store)

  let shallows t store =
    let fn uid =
      match store.read t uid with
      | Some (`A, payload) ->
          let parents = Mgit_object.parents_of_commit payload in
          List.exists (fun uid -> store.read t uid = None) parents
      | _ -> false in
    List.filter fn (commits t store)

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
    Option.iter (Negotiator.add_tip negotiator) (store.head t);
    negotiator

  let want t store refs =
    match List.assoc_opt (store.branch t) refs.Smart.refs with
    | Some hex -> Ok (store.branch t, Mgit_object.uid_of_hex_exn hex)
    | None ->
        begin match (store.is_empty t, refs.Smart.head_symref) with
        | true, Some name ->
            begin match List.assoc_opt name refs.Smart.refs with
            | Some hex -> Ok (name, Mgit_object.uid_of_hex_exn hex)
            | None -> error_msgf "%s does not exist on the remote" name
            end
        | _ -> error_msgf "%s does not exist on the remote" (store.branch t)
        end

  let receive t store ?deepen ~stateless flow ctx advertisement refs want into =
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
           Find_common.fetch_v1 ~stateless ~thin:true ~capabilities ~negotiator
             ~shallows ?deepen [ want ] q ctx in
     let result = run flow (reword fetch) in
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
    -> branch:string
    -> lookup:(uid -> 'tmp located option)
    -> reader:Mgit_change.read
    -> news:Mgit_change.news
    -> uid
    -> (unit, 'err) result

  type 'tmp tmp =
    { seq : len:int -> string Seq.t
    ; carton :
        len:int -> extern:(uid -> (Carton.Kind.t * Bstr.t) option) -> 'tmp Carton.t
    ; into : (string, unit) Flux.sink }

  let is_stateless edn =
    match edn.Endpoint.scheme with `HTTP | `HTTPS -> true | `Git | `SSH -> false

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
        end

  let pull t store ~generation ?deepen tmp = function
    | None -> error_msgf "No remote configured"
    | Some { ctx= remote_ctx; edn } ->
        let* flow, ctx = connect remote_ctx edn ~service:"git-upload-pack" ~version:2 in
        Fun.protect ~finally:(fun () -> Flow.close flow) @@ fun () ->
        let* advertisement = run flow (reword (Smart.advertisement ctx)) in
        let* refs =
          match advertisement with
          | Smart.V1 { refs; _ } -> Ok refs
          | Smart.V2 _ -> run flow (reword (Smart.ls_refs ctx)) in
        let* name, want = want t store refs in
        if Some want = store.head t then begin
          if not (is_stateless edn)
          then ignore (run flow (reword (Protocol.encode_flush_pkt ctx)));
          Ok []
        end
        else begin
          let before = paths t store in
          (* NOTE(dinosaure): download the PACK file into [tmp]. *)
          let stateless = is_stateless edn in
          let* len =
            receive t store ?deepen ~stateless flow ctx advertisement refs want
              tmp.into in
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
            let news = Mgit_change.make () in
            (* NOTE(dinosaure): save it. *)
            let* () = generation t ~branch:name ~lookup ~reader ~news want in
            let after = paths t store in
            Ok (diff ~before ~after)
          end
        end

  let push t store ~pack = function
    | None -> Ok ()
    | Some { ctx= remote_ctx; edn; _ } ->
        begin match store.head t with
        | None -> Ok ()
        | Some new_uid ->
            let* flow, ctx = connect remote_ctx edn ~service:"git-receive-pack" ~version:1 in
            Fun.protect ~finally:(fun () -> Flow.close flow) @@ fun () ->
            let* advertisement = run flow (reword (Smart.advertisement ctx)) in
            begin match advertisement with
            | Smart.V2 _ -> error_msgf "Unexpected protocol v2 from git-receive-pack"
            | Smart.V1 { refs; capabilities } ->
                let name = store.branch t in
                let old_uid =
                  Option.map Mgit_object.uid_of_hex_exn
                    (List.assoc_opt name refs.Smart.refs) in
                let fast_forward =
                  match old_uid with
                  | None -> true
                  | Some old_uid -> is_ancestor t store old_uid in
                if old_uid = Some new_uid then Ok ()
                else if not fast_forward then
                  error_msgf "%s: the remote has commits we do not have \
                              (non-fast-forward), pull first" name
                else
                  let* uids =
                    Mgit_closure.uncommon ~read:(store.read t)
                      ~exclude:(Option.to_list old_uid) new_uid in
                  Log.debug (fun m -> m "push %d object(s)" (List.length uids));
                  let* seq = pack uids in
                  let capabilities =
                    List.filter
                      (fun cap -> List.mem cap capabilities)
                      [ "report-status"; "ofs-delta" ] in
                  let commands = [ { Smart.old_uid; new_uid; name } ] in
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
                  | Some report ->
                      begin match
                        (report.Smart.unpack, List.assoc_opt name report.statuses)
                      with
                      | Error reason, _ ->
                          error_msgf "The remote refused our PACK file: %s" reason
                      | Ok (), Some (Error reason) ->
                          error_msgf "The remote refused to update %s: %s" name reason
                      | Ok (), (Some (Ok ()) | None) -> Ok ()
                      end
            end
        end
end
