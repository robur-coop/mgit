module Append = Append

(** [Blk] is a module which splits a block device into 3 parts:
    - a temporary part
    - two parts, one of which is the most up to date

    The aim is to be able to refer to a zone {i atomically} with consistent
    information. The old zone can be reused to apply an update which will (if
    all goes well) be treated as the new active zone once the atomic point is
    reached at which the changes to the [Blk] metadata are confirmed (see
    {!val:commit}). *)

type 'metadata t

val mapper : [ `Active | `Inactive | `Temporary ] -> 'm t Cachet.map
val append : 'm t -> [ `Active | `Inactive | `Temporary ] -> Append.t
val writer : 'm t -> [ `Active | `Inactive | `Temporary ] -> 'm t Cachet_wr.t
val source : 'm t -> [ `Active | `Inactive | `Temporary ] -> Bstr.t Flux.source
val sink : 'm t -> [ `Active | `Inactive | `Temporary ] -> (string, unit) Flux.sink

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
  -> Mkernel.Block.t
  -> ('m t, [> `Invalid_metadata | `Msg of string ]) result

val make :
     wr:('m -> Bstr.t -> int)
  -> rd:(Bstr.t -> ('m, [ `Invalid_metadata ]) result)
  -> Mkernel.Block.t
  -> ('m t, [> `Msg of string ]) result
