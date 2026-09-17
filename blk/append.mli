(** An append-only block device.

    This module lets the user to manipulate a block device as an append-only
    block device. In other words, bytes can only be appended up to a certain
    limit. If this limit is reached, the {!exception:Out_of_space} exception is
    raised. The bytes are written as soon as a sector can be written to the
    block device. If the user wishes to force the write, the {!val:flush}
    function is available (and the remaining bytes will be replaced by
    ["\000"]). Write operations to a sector are atomic. *)

exception Out_of_space

module type S = sig
  type t
  type blk

  val append : t -> ?off:int -> ?len:int -> string -> unit
  val flush : t -> unit
  val create : blk -> ?off:int -> int -> t
  val position : t -> int
  val written : t -> int
  val full : t -> bool
  val sink : init:(unit -> t) -> (string, unit) Flux.sink
end

module Make (Block : Device.S) : S with type blk = Block.t
