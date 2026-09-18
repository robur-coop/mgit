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

module Make (Flow : S) = struct
  let run flow t =
    let rec go = function
      | Protocol.Return value -> Ok value
      | Protocol.Error err -> Error err
      | Protocol.Read { buffer; off; len; k } ->
          begin match Flow.recv flow buffer ~off ~len with
          | Ok value -> go (k value)
          | Error (`Msg _ as err) -> Error err
          | Error _ as err -> err
          end
      | Protocol.Write { buffer; off; len; k } ->
          begin match Flow.send flow buffer ~off ~len with
          | Ok len -> go (k len)
          | Error (`Msg _ as err) -> Error err
          | Error _ as err -> err
          end in
    go t
end
