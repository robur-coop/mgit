val emit :
     ?level:int
  -> push:(string -> unit)
  -> load:(Carton.Uid.t -> 'meta -> Carton.Value.t)
  -> number_of_objects:int
  -> 'meta Cartonnage.Target.t Seq.t
  -> Classeur.Encoder.entry array * string

val idx : push:(string -> unit) -> pack:string -> Classeur.Encoder.entry array -> unit
(* [idx ~push ~pack entries], [pack] is the signature of the PACK file. *)
