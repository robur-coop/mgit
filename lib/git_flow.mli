type data = [ `End | `Len of int ]

module type S = sig
  type ctx
  type t

  val connect :
       ctx
    -> Endpoint.t
    -> service:string
    -> version:int
    -> (t, [> `Msg of string ]) result

  val recv : t -> bytes -> off:int -> len:int -> (data, [> `Msg of string ]) result
  val send : t -> string -> off:int -> len:int -> (int, [> `Msg of string ]) result
  val close : t -> unit
end

module Make (Flow : S) : sig
  val run :
    Flow.t -> ('a, ([> `Msg of string ] as 'err)) Protocol.t -> ('a, 'err) result
end
