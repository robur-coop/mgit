(* Abstraction of a block-device, a strict subset of [Mkernel.Block]. All the
   operations work on exactly one sector: [bstr] must contain [sector_size]
   bytes (starting at [src_off]/[dst_off] for the buffer side) and the offset
   on the device side must be a multiple of [sector_size].

   [blk] is functorised over this signature so that the very same code runs on
   Solo5 (via [Mkernel.Block]) and on a regular file (via [Block_unix], used by
   the test-suite and the command-line tool). *)

module type S = sig
  type t

  val sector_size : t -> int
  val length : t -> int
  val atomic_read : t -> src_off:int -> ?dst_off:int -> Bstr.t -> unit
  val atomic_write : t -> ?src_off:int -> dst_off:int -> Bstr.t -> unit
end
