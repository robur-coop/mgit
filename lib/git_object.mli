type error = [ `Msg of string ]

val pp_error : error Fmt.t

val ref_length : int (* [20], helper for [carton]. *)
val uid_of_hex : string -> (Carton.Uid.t, [> error ]) result
val uid_of_hex_exn : string -> Carton.Uid.t
val hex_of_uid : Carton.Uid.t -> string
val pp_uid : Carton.Uid.t Fmt.t
val digest : kind:Carton.Kind.t -> string -> Carton.Uid.t
val identify : Digestif.SHA1.ctx Carton.First_pass.identify
val digest_pack : unit -> Carton.First_pass.digest
val string_of_kind : Carton.Kind.t -> string
val kind_of_string : string -> (Carton.Kind.t, [> error ]) result

(** Users *)

module User : sig
  type t =
    { name : string
    ; email : string
    ; date : int * int option }

  val to_string : t -> string
  val of_string : string -> (t, [> error ]) result
  val pp : t Fmt.t
end

(** Trees *)

module Tree : sig
  type perm =
    [ `Normal
    | `Exec
    | `Everybody
    | `Link
    | `Dir
    | `Commit ]

  type entry = { perm : perm; name : string; node : Carton.Uid.t }
  type t

  val empty : t
  val v : entry list -> t
  val to_list : t -> entry list
  val is_empty : t -> bool
  val find : t -> string -> entry option
  val add : t -> entry -> t
  val remove : t -> string -> t
  val uids : t -> Carton.Uid.t list
  val of_string : string -> (t, [> error ]) result
  val to_string : t -> string
  val digest : t -> Carton.Uid.t
end

(** Commits *)

module Commit : sig
  type t =
    { tree : Carton.Uid.t
    ; parents : Carton.Uid.t list
    ; author : User.t
    ; committer : User.t
    ; extra : (string * string list) list
    ; message : string option }

  val make :
       tree:Carton.Uid.t
    -> ?parents:Carton.Uid.t list
    -> author:User.t
    -> committer:User.t
    -> ?extra:(string * string list) list
    -> string option
    -> t

  val of_string : string -> (t, [> error ]) result
  val to_string : t -> string
  val digest : t -> Carton.Uid.t
end

(** Tags *)

module Tag : sig
  type t =
    { obj : Carton.Uid.t
    ; kind : Carton.Kind.t
    ; tag : string
    ; tagger : User.t option
    ; message : string option }

  val of_string : string -> (t, [> error ]) result
  val to_string : t -> string
  val digest : t -> Carton.Uid.t
end

val tree_of_commit : string -> Carton.Uid.t option
val parents_of_commit : string -> Carton.Uid.t list
val target_of_tag : string -> Carton.Uid.t option
val entries : string -> (Tree.entry list, [> error ]) result
val links : kind:Carton.Kind.t -> string -> Carton.Uid.t list
