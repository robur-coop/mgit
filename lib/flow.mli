module type S = sig
  type t
  type ctx

  val connect : ctx -> Endpoint.t -> (t, [> `Msg of string ]) result

  val recv :
    t -> bytes -> off:int -> len:int
    -> ([ `End | `Len of int ], [> `Msg of string ]) result

  val send : t -> string -> off:int -> len:int -> (int, [> `Msg of string ]) result
  val close : t -> unit
end

module Make (Flow : S) : sig
  val run :
    Flow.t -> ('a, ([> `Msg of string ] as 'err)) Protocol.t -> ('a, 'err) result
end
