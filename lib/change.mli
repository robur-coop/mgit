type read = Carton.Uid.t -> (Carton.Kind.t * string) option
type news
type error = [ `Msg of string ]
type t = string list * [ `Set of Git_object.Tree.perm * string | `Rem ]

val make : unit -> news
val find : news -> Carton.Uid.t -> (Carton.Kind.t * string) option
val read_with : read -> news -> Carton.Uid.t -> (Carton.Kind.t * string) option
val add : news -> Carton.Kind.t -> string -> Carton.Uid.t
val root : read:read -> news:news -> Carton.Uid.t option -> t list -> (Carton.Uid.t, [> error ]) result
