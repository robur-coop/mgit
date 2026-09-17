(* NOTE(dinosaure): see [Mkernel.Block] *)

module type S = sig
  type t

  val sector_size : t -> int
  val length : t -> int
  val atomic_read : t -> src_off:int -> ?dst_off:int -> Bstr.t -> unit
  val atomic_write : t -> ?src_off:int -> dst_off:int -> Bstr.t -> unit
end
