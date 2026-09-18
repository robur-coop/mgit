type error =
  [ `Msg of string
  | `Zone_full
  | `Not_found of Carton.Uid.t ]

val pp_error : error Fmt.t

module Make (Block : Blk.BLOCK) : sig
  type t
  type fd

  val format : ?ratio:float -> ?length:int -> Block.t -> (unit, [> error ]) result
  val load : Block.t -> (t, [> error ]) result
  val references : t -> (string * Carton.Uid.t) list
  val prerequisites : t -> (Carton.Uid.t * string option) list
  val reference : t -> string -> Carton.Uid.t option
  val is_empty : t -> bool
  val exists : t -> Carton.Uid.t -> bool
  val uids : t -> Carton.Uid.t list
  val kind : t -> Carton.Uid.t -> Carton.Kind.t option
  val length : t -> Carton.Uid.t -> int option
  val value : t -> Carton.Uid.t -> Carton.Value.t option
  val read : t -> Carton.Uid.t -> (Carton.Kind.t * string) option

  val publish :
       t
    -> ?level:int
    -> references:(string * Carton.Uid.t) list
    -> prerequisites:(Carton.Uid.t * string option) list
    -> load:(Carton.Uid.t -> 'meta -> Carton.Value.t)
    -> 'meta Cartonnage.Entry.t list
    -> (t, [> error ]) result

  val to_bundle : t -> string Seq.t

  module Tmp : sig
    type extern = Carton.Uid.t -> (Carton.Kind.t * Bstr.t) option

    val sink : t -> (string, unit) Flux.sink
    val seq : t -> len:int -> string Seq.t
    val carton : ?extern:extern -> t -> len:int -> fd Carton.t
  end
end
