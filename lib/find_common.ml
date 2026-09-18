(* NOTE(dinosaure): see [find_common.ml] in [ocaml-git]. *)

let src = Logs.Src.create "mgit.find-common"

module Log = (val Logs.src_log src : Logs.LOG)

let ( let* ) = Protocol.bind
let return = Protocol.return

let initial_flush = 16
let pipe_safe_flush = 32
let large_flush = 16384
let max_in_vain = 256

let next_flush ~stateless count =
  if stateless then if count < large_flush then count lsl 1 else count * 11 / 10
  else if count < pipe_safe_flush then count lsl 1
  else count + pipe_safe_flush

let hex = Smart.hex

let is_common = function `ACK_common _ -> true | _ -> false

let find_common ~stateless ~multi_ack ~no_done ~caps ~negotiator ~shallows
    ?deepen wants ctx =
  match wants with
  | [] ->
      let* () = Protocol.encode_flush_pkt ctx in
      return (`Close, [])
  | first :: rest ->
      let* () =
        Protocol.encode_pkt ctx "want %s%s\n" (hex first)
          (String.concat "" (List.map (fun cap -> " " ^ cap) caps)) in
      let* () =
        Smart.iter (fun uid -> Protocol.encode_pkt ctx "want %s\n" (hex uid)) rest in
      let* () =
        Smart.iter (fun uid -> Protocol.encode_pkt ctx "shallow %s" (hex uid)) shallows in
      let* () =
        match deepen with
        | Some depth -> Protocol.encode_pkt ctx "deepen %d" depth
        | None -> return () in
      let* () = Protocol.encode_flush_pkt ctx in
      let* updates =
        match deepen with Some _ -> Smart.shallow_list ctx | None -> return [] in
      let consume_shallow_list () =
        if stateless && Option.is_some deepen
        then let* _ = Smart.shallow_list ctx in return ()
        else return () in
      let count = ref 0 and flushes = ref 0 and flush_at = ref initial_flush in
      let in_vain = ref 0 and got_continue = ref false and got_ready = ref false in
      let retval = ref (-1) in
      let rec negotiate () =
        match Negotiator.next negotiator with
        | None -> return ()
        | Some uid ->
            let* () = Protocol.encode_pkt ctx "have %s\n" (hex uid) in
            incr in_vain;
            incr count;
            if !flush_at > !count then negotiate ()
            else begin
              let* () = Protocol.encode_flush_pkt ctx in
              incr flushes;
              flush_at := next_flush ~stateless !count;
              if (not stateless) && !count = initial_flush then negotiate ()
              else
                let* () = consume_shallow_list () in
                let rec acks () =
                  let* ack = Smart.get_ack ctx in
                  match ack with
                  | `NAK -> return `Continue
                  | `ACK _ ->
                      flushes := 0;
                      multi_ack := `None;
                      retval := 0;
                      return `Done
                  | (`ACK_common uid | `ACK_ready uid | `ACK_continue uid) as ack ->
                      let was_common = Negotiator.ack negotiator uid in
                      let* () =
                        if stateless && is_common ack && not was_common then begin
                          in_vain := 0;
                          Protocol.encode_pkt ctx "have %s\n" (hex uid)
                        end
                        else begin
                          if (not stateless) || not (is_common ack)
                          then in_vain := 0;
                          return ()
                        end in
                      retval := 0;
                      got_continue := true;
                      (match ack with `ACK_ready _ -> got_ready := true | _ -> ());
                      acks () in
                let* result = acks () in
                match result with
                | `Done -> return ()
                | `Continue ->
                    decr flushes;
                    if !got_continue && max_in_vain < !in_vain
                    then begin Log.debug (fun m -> m "giving up"); return () end
                    else if !got_ready then return ()
                    else negotiate ()
            end in
      let* () = negotiate () in
      let* () =
        if (not !got_ready) || not no_done
        then Protocol.encode_pkt ctx "done\n"
        else return () in
      if !retval <> 0 then (multi_ack := `None; incr flushes);
      let* () =
        if (not !got_ready) || not no_done then consume_shallow_list ()
        else return () in
      let rec finish () =
        if !flushes > 0 || !multi_ack <> `None then
          let* ack = Smart.get_ack ctx in
          match ack with
          | `ACK _ -> return 0
          | `ACK_common _ | `ACK_continue _ | `ACK_ready _ ->
              multi_ack := `Some;
              finish ()
          | `NAK -> decr flushes; finish ()
        else return !retval in
      let* retval = finish () in
      if retval <> 0 then Log.debug (fun m -> m "No common commits");
      return (`Continue, updates)

let has cap capabilities = List.mem cap capabilities

let fetch_v1 ?(stateless = false) ?(thin = false) ~capabilities ~negotiator
    ~shallows ?deepen wants q ctx =
  let side_band =
    if has "side-band-64k" capabilities then Some "side-band-64k"
    else if has "side-band" capabilities then Some "side-band"
    else None in
  match side_band with
  | None -> Protocol.error `No_side_band
  | Some side_band ->
      if (shallows <> [] || deepen <> None) && not (has "shallow" capabilities)
      then Protocol.error (`Err "the remote does not support shallow clients")
      else begin
        let multi_ack =
          ref
            (if has "multi_ack_detailed" capabilities then `Detailed
             else if has "multi_ack" capabilities then `Some
             else `None) in
        let no_done = stateless && has "no-done" capabilities in
        let caps =
          (match !multi_ack with
           | `Detailed -> [ "multi_ack_detailed" ]
           | `Some -> [ "multi_ack" ]
           | `None -> [])
          @ (if no_done then [ "no-done" ] else [])
          @ [ side_band ]
          @ (if thin && has "thin-pack" capabilities then [ "thin-pack" ] else [])
          @ List.filter (fun cap -> has cap capabilities) [ "no-progress"; "ofs-delta" ]
        in
        let* result, updates =
          find_common ~stateless ~multi_ack ~no_done ~caps ~negotiator ~shallows
            ?deepen wants ctx in
        match result with
        | `Close -> return (updates, false)
        | `Continue ->
            let* errored = Smart.side_band false q ctx in
            return (updates, errored)
      end

let feature ~command name capabilities =
  let prefix = command ^ "=" in
  let values ~prefix str =
    String.sub str (String.length prefix) (String.length str - String.length prefix) in
  let fn cap =
    String.starts_with ~prefix cap
    && let values = values ~prefix cap in
       List.mem name (String.split_on_char ' ' values) in
  List.exists fn capabilities

let send_fetch_request ~thin ~negotiator ~shallows ?deepen ~wants ~common
    ~haves_to_send ~in_vain ~seen_ack ctx =
  let* () = Protocol.encode_pkt ctx "command=fetch" in
  let* () = Protocol.encode_pkt ctx "object-format=sha1" in
  let* () = Protocol.encode_delim_pkt ctx in
  let* () = if thin then Protocol.encode_pkt ctx "thin-pack" else return () in
  let* () = Protocol.encode_pkt ctx "no-progress" in
  let* () = Protocol.encode_pkt ctx "ofs-delta" in
  let* () = Smart.iter (fun uid -> Protocol.encode_pkt ctx "shallow %s" (hex uid)) shallows in
  let* () =
    match deepen with
    | Some depth -> Protocol.encode_pkt ctx "deepen %d" depth
    | None -> return () in
  let* () = Smart.iter (fun uid -> Protocol.encode_pkt ctx "want %s\n" (hex uid)) wants in
  let* () =
    Smart.iter (fun uid -> Protocol.encode_pkt ctx "have %s\n" (hex uid)) (List.rev common) in
  let rec add_haves added =
    if added >= !haves_to_send then return added
    else
      match Negotiator.next negotiator with
      | None -> return added
      | Some uid ->
          let* () = Protocol.encode_pkt ctx "have %s\n" (hex uid) in
          add_haves (added + 1) in
  let* haves_added = add_haves 0 in
  haves_to_send := next_flush ~stateless:true !haves_to_send;
  in_vain := !in_vain + haves_added;
  let done_sent = haves_added = 0 || (seen_ack && !in_vain >= max_in_vain) in
  let* () = if done_sent then Protocol.encode_pkt ctx "done\n" else return () in
  let* () = Protocol.encode_flush_pkt ctx in
  return done_sent

let expect_section ctx section =
  let* packet = Protocol.decode_pkt_or_delim_or_end ctx in
  match packet with
  | `Line line when String.trim line = section -> return ()
  | `Line line ->
      begin match Smart.err_of_pkt (String.trim line) with
      | Some msg -> Protocol.error (`Err msg)
      | None ->
          Log.err (fun m -> m "Expected %S, received %S" section line);
          Protocol.error `Invalid_pkt_line
      end
  | _ ->
      Log.err (fun m -> m "Expected %S" section);
      Protocol.error `Invalid_pkt_line

let process_acks ~negotiator ctx =
  let rec go common ready =
    let* packet = Protocol.decode_pkt_or_delim_or_end ctx in
    match packet with
    | `Delim when ready -> return (List.rev common, ready)
    | `Flush when not ready -> return (List.rev common, ready)
    | `Flush | `Delim | `End ->
        Log.err (fun m -> m "Unexpected end of the acknowledgments section");
        Protocol.error `Invalid_pkt_line
    | `Line line ->
        let line = String.trim line in
        if line = "NAK" then go common ready
        else if line = "ready" then go common true
        else if String.starts_with ~prefix:"ACK " line then
          match Smart.uid_of_hex_opt (String.sub line 4 (String.length line - 4)) with
          | Some uid ->
              let _was_common = Negotiator.ack negotiator uid in
              go (uid :: common) ready
          | None -> Protocol.error `Invalid_pkt_line
        else begin
          Log.err (fun m -> m "Unexpected acknowledgment line: %S" line);
          Protocol.error `Invalid_pkt_line
        end in
  go [] false

let rec skip_section ctx =
  let* packet = Protocol.decode_pkt_or_delim_or_end ctx in
  match packet with
  | `Line _ -> skip_section ctx
  | `Flush | `Delim | `End -> return ()

let rec sections updates ctx =
  let* packet = Protocol.decode_pkt_or_delim_or_end ctx in
  match packet with
  | `Line line ->
      begin match String.trim line with
      | "packfile" -> return updates
      | "shallow-info" ->
          let* updates = Smart.shallow_list ctx in
          sections updates ctx
      | "wanted-refs" | "packfile-uris" ->
          let* () = skip_section ctx in
          sections updates ctx
      | line ->
          begin match Smart.err_of_pkt line with
          | Some msg -> Protocol.error (`Err msg)
          | None ->
              Log.err (fun m -> m "Unexpected section: %S" line);
              Protocol.error `Invalid_pkt_line
          end
      end
  | `Flush | `Delim | `End -> sections updates ctx

let fetch_v2 ?(thin = false) ~capabilities ~negotiator ~shallows ?deepen wants
    q ctx =
  if (shallows <> [] || deepen <> None)
     && not (feature ~command:"fetch" "shallow" capabilities)
  then Protocol.error (`Err "the remote does not support shallow requests")
  else if wants = [] then return ([], false)
  else begin
    let haves_to_send = ref initial_flush and in_vain = ref 0 in
    let rec send_request ~common ~seen_ack =
      let* done_sent =
        send_fetch_request ~thin ~negotiator ~shallows ?deepen ~wants ~common
          ~haves_to_send ~in_vain ~seen_ack ctx in
      if done_sent then get_pack ()
      else
        let* () = expect_section ctx "acknowledgments" in
        let* acked, ready = process_acks ~negotiator ctx in
        if acked <> [] then in_vain := 0;
        let common = List.rev_append acked common in
        let seen_ack = seen_ack || acked <> [] in
        if ready then get_pack () else send_request ~common ~seen_ack
    and get_pack () =
      let* updates = sections [] ctx in
      let* errored = Smart.side_band false q ctx in
      return (updates, errored) in
    send_request ~common:[] ~seen_ack:false
  end
