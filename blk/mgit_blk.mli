module Device = Device
module Append = Append

module type BLOCK = Device.S

(** [Blk] is a module which splits a block device into 3 parts:
    - a temporary part
    - two parts, one of which is the most up to date

    The aim is to be able to refer to a zone {i atomically} with consistent
    information. The old zone can be reused to apply an update which will (if
    all goes well) be treated as the new active zone once the atomic point is
    reached at which the changes to the [Blk] metadata are confirmed (see
    {!val:commit}). *)

type zone =
  [ `Active
  | `Inactive
  | `Temporary ]

module Make (Block : BLOCK) : sig
  module Append : module type of Append.Make (Block)

  type 'metadata t

  val mapper : zone -> 'm t Cachet.map
  val append : 'm t -> zone -> Append.t
  val writer : 'm t -> zone -> 'm t Cachet_wr.t
  val sink : 'm t -> zone -> (string, unit) Flux.sink

  val bounds : 'm t -> zone -> int * int
  (** [bounds t which] is the absolute offset and the length (in bytes) of the
      requested zone on the block-device. *)

  val cachet : 'm t -> zone -> base:int -> len:int -> 'm t Cachet.t
  (** [cachet t which ~base ~len] is a {!Cachet.t} whose logical address [0]
      is the byte [base] of the zone [which] and which never reads further than
      [base + len]. Out of bounds reads return an empty bigstring, as
      {!type:Cachet.map} requires. *)

  val seq : 'm t -> zone -> ?off:int -> ?len:int -> unit -> string Seq.t
  (** [seq t which ~off ~len ()] streams [len] bytes of the zone [which],
      starting at the byte [off] of that zone. *)

  val source : 'm t -> zone -> ?off:int -> ?len:int -> unit -> string Flux.source

  val metadata : 'm t -> 'm
  val with_metadata : 'm t -> 'm -> 'm t

  val sync : 'm t -> 'm t
  (** [sync t] upgrades the current active zone and atomically save it into the
      block device. *)

  val commit : 'm t -> 'm t
  (** [commit t] sets the current active zone (the inactive zone becomes active
      and vice-versa) and atomically save it into the block device. *)

  val format :
       ?ratio:float
    -> rd:(Bstr.t -> ('m, [ `Invalid_metadata ]) result)
    -> wr:('m -> Bstr.t -> int)
    -> ?length:int
    -> Block.t
    -> ('m t, [> `Invalid_metadata | `Msg of string ]) result

  val make :
       wr:('m -> Bstr.t -> int)
    -> rd:(Bstr.t -> ('m, [ `Invalid_metadata ]) result)
    -> Block.t
    -> ('m t, [> `Msg of string ]) result
end
