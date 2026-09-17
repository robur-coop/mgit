(* Like [Carton_miou_unix.delta] *)

let src = Logs.Src.create "mgit.delta"

module Log = (val Logs.src_log src : Logs.LOG)

module Window = struct
  type 'meta t =
    { arr : 'meta Cartonnage.Source.t array
    ; mutable rd_pos : int
    ; mutable wr_pos : int }

  let make () = { arr= Array.make 0x100 (Obj.magic ()); rd_pos= 0; wr_pos= 0 }
  let is_full { rd_pos; wr_pos; arr } = wr_pos - rd_pos = Array.length arr
end

let max_depth = 50

let should_we_apply ~source entry =
  let open Cartonnage in
  let size_guessed =
    match Target.patch entry with
    | None -> Target.length entry / 3
    | Some patch -> Patch.length patch / 3 in
  if Source.length source < Target.length entry then false
  else
    let diff = Source.length source - Target.length entry in
    diff < size_guessed

let apply ~load ~window:t entry =
  let len = t.Window.wr_pos - t.Window.rd_pos in
  let msk = Array.length t.Window.arr - 1 in
  let uid = Cartonnage.Target.uid entry
  and meta = Cartonnage.Target.meta entry in
  let target = Lazy.from_fun (fun () -> load uid meta) in
  for i = 0 to len - 1 do
    let source = t.Window.arr.((t.Window.rd_pos + i) land msk) in
    if Cartonnage.Source.depth source < max_depth
       && should_we_apply ~source entry
    then Cartonnage.Target.diff entry ~source ~target:(Lazy.force target)
  done;
  if Lazy.is_val target || Cartonnage.Target.depth entry == 1
  then Some (Lazy.force target)
  else None

let append ~window:t source =
  let open Window in
  let msk = Array.length t.arr - 1 in
  match Array.length t.arr - (t.wr_pos - t.rd_pos) with
  | 0 ->
      t.arr.(t.rd_pos land msk) <- source;
      t.rd_pos <- t.rd_pos + 1;
      t.wr_pos <- t.wr_pos + 1
  | _ ->
      t.arr.(t.wr_pos land msk) <- source;
      t.wr_pos <- t.wr_pos + 1

let remember ~window entry ~target =
  if Cartonnage.Target.depth entry < max_depth then begin
    let source = Cartonnage.Target.to_source entry ~target in
    Log.debug (fun m ->
        m "add %a as a possible source (depth: %d)" Carton.Uid.pp
          (Cartonnage.Source.uid source)
          (Cartonnage.Target.depth entry));
    append ~window source
  end

let delta ~load =
  let windows = Array.init 4 (fun _ -> Window.make ()) in
  Seq.map @@ fun entry ->
  let entry = Cartonnage.Target.make entry in
  let k = Carton.Kind.to_int (Cartonnage.Target.kind entry) in
  let window = windows.(k) in
  begin match (apply ~load ~window entry, Window.is_full window) with
  | None, true -> ()
  | None, false ->
      if Cartonnage.Target.depth entry < max_depth then begin
        let uid = Cartonnage.Target.uid entry
        and meta = Cartonnage.Target.meta entry in
        remember ~window entry ~target:(load uid meta)
      end
  | Some target, _ -> remember ~window entry ~target
  end;
  entry
