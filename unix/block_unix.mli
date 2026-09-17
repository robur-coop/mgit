(** A block device as an unix file. *)

type t

val create : ?sector_size:int -> string -> int -> (t, [> `Msg of string ]) result
val load : ?sector_size:int -> string -> (t, [> `Msg of string ]) result
val close : t -> unit

include Blk.BLOCK with type t := t
