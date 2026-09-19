module type CLIENT = sig
  type ctx

  val request :
       ctx
    -> meth:[ `GET | `POST ]
    -> headers:(string * string) list
    -> ?body:string Seq.t
    -> uri:string
    -> (string -> unit)
    -> (unit, [> `Msg of string ]) result
end

module Make (Client : CLIENT) : Mgit_sync.S with type ctx = Client.ctx
