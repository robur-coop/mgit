type scheme =
  [ `Git
  | `SSH
  | `HTTP
  | `HTTPS ]

type t =
  { scheme : scheme
  ; user : string option
  ; host : string
  ; port : int option
  ; path : string
  ; branch : string option }

val of_string : string -> (t, [> `Msg of string ]) result
val to_string : t -> string
val uri : t -> string
val port : t -> int
val pp : t Fmt.t
