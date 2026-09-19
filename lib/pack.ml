let src = Logs.Src.create "mgit.pack"

module Log = (val Logs.src_log src : Logs.LOG)
module SHA1 = Digestif.SHA1

let be32 v =
  let buf = Bytes.create 4 in
  Bytes.set_int32_be buf 0 (Int32.of_int v);
  Bytes.unsafe_to_string buf

let buffers () =
  let o = Bstr.create 0x7ff
  and i = Bstr.create 0x7ff
  and q = De.Queue.create 0x10_000
  and w = De.Lz77.make_window ~bits:15 in
  { Cartonnage.o; i; q; w }

let to_seq ?level ?(on_entry = fun ~uid:_ ~offset:_ ~crc:_ -> ()) ~load
    ~number_of_objects targets =
  let buffers = buffers () in
  let o = buffers.Cartonnage.o in
  let ctx = ref SHA1.empty in
  let chunk str = ctx := SHA1.feed_string !ctx str; str in
  let emitted = Hashtbl.create 0x100 in
  let where uid = Hashtbl.find_opt emitted uid in
  let cursor = ref 12 in
  let entry target () =
    let uid = Cartonnage.Target.uid target
    and meta = Cartonnage.Target.meta target in
    let value = load uid meta in
    let offset = !cursor in
    let crc = ref Checkseum.Crc32.default in
    let _hdr_len, encoder =
      Cartonnage.encode ?level ~buffers ~where target ~target:value
        ~cursor:offset in
    let rec go encoder () =
      match Cartonnage.Encoder.encode ~o encoder with
      | `Flush (encoder, len) ->
          crc := Checkseum.Crc32.digest_bigstring o 0 len !crc;
          let str = chunk (Bstr.sub_string o ~off:0 ~len) in
          cursor := !cursor + len;
          let encoder = Cartonnage.Encoder.dst encoder o 0 (Bstr.length o) in
          Seq.Cons (str, go encoder)
      | `End ->
          Log.debug (fun m ->
              m "%a emitted at %08x (%d byte(s))" Carton.Uid.pp uid offset
                (!cursor - offset));
          Hashtbl.replace emitted uid offset;
          on_entry ~uid ~offset ~crc:!crc;
          Seq.Nil in
    go encoder () in
  let header () =
    Seq.Cons (chunk ("PACK" ^ be32 2 ^ be32 number_of_objects), Seq.empty) in
  let signature () =
    Seq.Cons (SHA1.to_raw_string (SHA1.get !ctx), Seq.empty) in
  Seq.append header
    (Seq.append (Seq.flat_map (fun target -> entry target) targets) signature)

let emit ?level ~push ~load ~number_of_objects targets =
  let entries = ref [] in
  let on_entry ~uid ~offset ~crc =
    let uid = Classeur.unsafe_uid_of_string (uid : Carton.Uid.t :> string) in
    let entry = { Classeur.Encoder.crc; offset= Int64.of_int offset; uid } in
    entries := entry :: !entries in
  let hash = ref "" in
  let seq = to_seq ?level ~on_entry ~load ~number_of_objects targets in
  (* the last element is the signature *)
  Seq.iter (fun str -> hash := str; push str) seq;
  let entries = Array.of_list !entries in
  let compare { Classeur.Encoder.uid= a; _ } { Classeur.Encoder.uid= b; _ } =
    String.compare (a :> string) (b :> string) in
  Array.sort compare entries;
  (entries, !hash)

let digest () =
  let feed_bytes buf ~off ~len ctx = SHA1.feed_bytes ctx ~off ~len buf in
  let feed_bigstring bstr ctx = SHA1.feed_bigstring ctx bstr in
  let serialize ctx = SHA1.to_raw_string (SHA1.get ctx) in
  let hash =
    { Carton.First_pass.feed_bytes; feed_bigstring; serialize
    ; length= SHA1.digest_size } in
  Carton.First_pass.Digest (hash, SHA1.empty)

let idx ~push ~pack entries =
  let buf = Buffer.create 0x1000 in
  let encoder =
    Classeur.Encoder.encoder (`Buffer buf) ~digest:(digest ()) ~pack
      ~ref_length:SHA1.digest_size entries in
  begin match Classeur.Encoder.encode encoder `Await with
  | `Ok -> ()
  | `Partial -> assert false (* [`Buffer] never returns [`Partial] *)
  end;
  push (Buffer.contents buf)
