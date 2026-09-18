(* NOTE(dinosaure): it's like [negociator.c] in Git and
   [neg.ml]/[find_common.ml] in [ocaml-git]. *)

let common = 1 lsl 2
let common_ref = 1 lsl 3
let seen = 1 lsl 4
let popped = 1 lsl 5

type commit =
  { uid : Carton.Uid.t
  ; date : int
  ; parents : Carton.Uid.t list
  ; mutable flags : int }

module Queue = struct
  type t = { mutable arr : (commit * int) array; mutable len : int; mutable ctr : int }

  let make () = { arr= [||]; len= 0; ctr= 0 }
  let is_empty t = t.len = 0

  let higher (a, ia) (b, ib) =
    if a.date <> b.date then a.date > b.date else ia < ib

  let swap t i j =
    let v = t.arr.(i) in
    t.arr.(i) <- t.arr.(j);
    t.arr.(j) <- v

  let push t commit =
    let v = (commit, t.ctr) in
    t.ctr <- t.ctr + 1;
    if t.len = Array.length t.arr then begin
      let arr = Array.make (Int.max 16 (2 * t.len)) v in
      Array.blit t.arr 0 arr 0 t.len;
      t.arr <- arr
    end;
    t.arr.(t.len) <- v;
    let rec up i =
      let p = (i - 1) / 2 in
      if i > 0 && higher t.arr.(i) t.arr.(p) then (swap t i p; up p) in
    up t.len;
    t.len <- t.len + 1

  let pop t =
    if t.len = 0 then None
    else begin
      let commit, _ = t.arr.(0) in
      t.len <- t.len - 1;
      t.arr.(0) <- t.arr.(t.len);
      let rec down i =
        let l = (2 * i) + 1 and r = (2 * i) + 2 in
        let m = if l < t.len && higher t.arr.(l) t.arr.(i) then l else i in
        let m = if r < t.len && higher t.arr.(r) t.arr.(m) then r else m in
        if m <> i then (swap t i m; down m) in
      down 0;
      Some commit
    end
end

type t =
  { rev_list : Queue.t
  ; mutable non_common_revs : int
  ; commits : (string, commit option) Hashtbl.t
  ; load : Carton.Uid.t -> (int * Carton.Uid.t list) option }

let make ~load =
  { rev_list= Queue.make (); non_common_revs= 0; commits= Hashtbl.create 0x100; load }

let lookup t uid =
  let key = (uid : Carton.Uid.t :> string) in
  match Hashtbl.find_opt t.commits key with
  | Some value -> value
  | None ->
      let value =
        Option.map
          (fun (date, parents) -> { uid; date; parents; flags= 0 })
          (t.load uid) in
      Hashtbl.replace t.commits key value;
      value

let parents t commit = List.filter_map (lookup t) commit.parents

let rev_list_push t commit mark =
  if commit.flags land mark = 0 then begin
    commit.flags <- commit.flags lor mark;
    Queue.push t.rev_list commit;
    if commit.flags land common = 0 then t.non_common_revs <- t.non_common_revs + 1
  end

let mark_common t commit ~ancestors_only =
  if commit.flags land common = 0 then begin
    let stack = Stack.create () in
    Stack.push commit stack;
    if not ancestors_only then begin
      commit.flags <- commit.flags lor common;
      if commit.flags land seen <> 0 && commit.flags land popped = 0
      then t.non_common_revs <- t.non_common_revs - 1
    end;
    while not (Stack.is_empty stack) do
      let commit = Stack.pop stack in
      if commit.flags land seen = 0 then rev_list_push t commit seen
      else
        let fn p =
          if p.flags land common = 0 then begin
            p.flags <- p.flags lor common;
            if p.flags land seen <> 0 && p.flags land popped = 0
            then t.non_common_revs <- t.non_common_revs - 1;
            Stack.push p stack
          end in
        List.iter fn (parents t commit)
    done
  end

let rec get_rev t =
  if Queue.is_empty t.rev_list || t.non_common_revs = 0 then None
  else
    match Queue.pop t.rev_list with
    | None -> None
    | Some commit ->
        let ps = parents t commit in
        commit.flags <- commit.flags lor popped;
        if commit.flags land common = 0
        then t.non_common_revs <- t.non_common_revs - 1;
        let result, mark =
          if commit.flags land common <> 0
          then (* NOTE(dinosaure): do not send "have", and ignore ancestors *)
            (None, common lor seen)
          else if commit.flags land common_ref <> 0
          then (* NOTE(dinosaure): send "have", and ignore ancestors *)
            (Some commit.uid, common lor seen)
          else (* NOTE(dinosaure): send "have", also for its ancestors *)
            (Some commit.uid, seen) in
        let fn p =
          if p.flags land seen = 0 then rev_list_push t p mark;
          if mark land common <> 0 then mark_common t p ~ancestors_only:true in
        List.iter fn ps;
        begin match result with None -> get_rev t | Some _ -> result end

let known_common t uid =
  match lookup t uid with
  | Some c when c.flags land seen = 0 ->
      rev_list_push t c (common_ref lor seen);
      mark_common t c ~ancestors_only:true
  | _ -> ()

let add_tip t uid =
  match lookup t uid with Some c -> rev_list_push t c seen | None -> ()

let next t = get_rev t

let ack t uid =
  match lookup t uid with
  | None -> false
  | Some c ->
      let known = c.flags land common <> 0 in
      mark_common t c ~ancestors_only:false;
      known

let have_sent t uid =
  match lookup t uid with
  | Some c -> mark_common t c ~ancestors_only:false
  | None -> ()
