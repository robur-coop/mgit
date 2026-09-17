type mem
type blk
type uid = private string

type 'a t

type error =
  [ `Msg of string
  | `Not_found of string ]

type 'a from =
  | Blk : string -> blk from
  | Mem : mem from
  | Net : Mnet_happy_eyeballs.t -> mem from

val make : ?branch:string -> 'k from -> 'k t

type change =
  [ `Add of string
  | `Rem of string
  | `Set of string ]

val pull : 'k t -> (change list, [> `Msg of string ]) result

val change_and_push :
  'k t
  -> ?author:string
  -> ?email:string
  -> ?message:string
  -> ('k t -> 'a)
  -> ('a, [> `Msg of string ]) result

type perm =
  [ `Normal
  | `Exec
  | `Everybody
  | `Link ]

val get : 'k t -> string -> (perm * string, [> error ]) result
val set : 'k t -> ?perm:perm -> string -> string -> (unit, [> error ]) result
val branch : 'k t -> string
val commit : 'k t -> [ `Clean of uid | `Dirty of uid ] option
val gc : ?shallows:int -> 'k t -> unit
