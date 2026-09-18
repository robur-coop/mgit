type t =
  { mutable objects : (string, Carton.Kind.t * string) Hashtbl.t
  ; mutable references : (string * Carton.Uid.t) list
  ; mutable prerequisites : (Carton.Uid.t * string option) list
  ; tmp : Buffer.t }

let make () =
  { objects= Hashtbl.create 0x100
  ; references= []
  ; prerequisites= []
  ; tmp= Buffer.create 0x1000 }

let references t = t.references
let prerequisites t = t.prerequisites
let reference t name = List.assoc_opt name t.references
let is_empty t = t.references = []
let read t (uid : Carton.Uid.t) = Hashtbl.find_opt t.objects (uid :> string)
let kind t uid = Option.map fst (read t uid)
let length t uid = Option.map (fun (_, payload) -> String.length payload) (read t uid)

let value t uid =
  Option.map (fun (kind, payload) -> Carton.Value.of_string ~kind payload) (read t uid)

let uids t =
  List.of_seq (Seq.map Carton.Uid.unsafe_of_string (Hashtbl.to_seq_keys t.objects))

let publish t ~references ~prerequisites objects =
  let tbl = Hashtbl.create (List.length objects) in
  let fn ((uid : Carton.Uid.t), kind, payload) =
    Hashtbl.replace tbl (uid :> string) (kind, payload) in
  List.iter fn objects;
  t.objects <- tbl;
  t.references <- references;
  t.prerequisites <- prerequisites

module Tmp = struct
  type extern = Carton.Uid.t -> (Carton.Kind.t * Bstr.t) option

  let sink t =
    let init () = Buffer.clear t.tmp; t.tmp in
    let push buf str = Buffer.add_string buf str; buf in
    Flux.Sink { init; push; full= Fun.const false; stop= ignore }

  let seq t ~len =
    let rec go off () =
      if off >= len then Seq.Nil
      else
        let n = Int.min 0x1000 (len - off) in
        Seq.Cons (Buffer.sub t.tmp off n, go (off + n)) in
    go 0

  let map bstr ~pos len =
    let max = Bstr.length bstr in
    if pos < 0 || pos >= max then Bstr.empty
    else Bstr.sub bstr ~off:pos ~len:(Int.min len (max - pos))

  let carton ?(extern = Fun.const None) t ~len =
    let bstr = Bstr.of_string (Buffer.sub t.tmp 0 len) in
    let index (uid : Carton.Uid.t) =
      match extern uid with
      | Some (kind, bstr) -> Carton.Extern (kind, bstr)
      | None -> raise Not_found in
    Carton.make ~map bstr ~z:(Bstr.create 0x1000)
      ~allocate:(fun bits -> De.make_window ~bits)
      ~ref_length:Git_object.ref_length index
end
