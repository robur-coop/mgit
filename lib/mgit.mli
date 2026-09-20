type error =
  [ `Msg of string
  | `Out_of_space  (** The block-device is too small for the new generation. *)
  | `Not_found of string  (** The given path does not exist. *) ]

val pp_error : error Fmt.t

type perm =
  [ `Normal  (** [100644] *)
  | `Exec  (** [100755] *)
  | `Everybody  (** [100664] *)
  | `Link  (** [120000], the contents is the target of the link. *) ]

type change =
  [ `Add of string  (** A new file. *)
  | `Rem of string  (** A file which disappeared. *)
  | `Set of string  (** A file whose contents changed. *) ]

type uid = private string

val uid_of_hex : string -> (uid, [> `Msg of string ]) result
(** [uid_of_hex hex] is the identifier written [hex] (as git prints it). *)

val uid_to_hex : uid -> string
(** [uid_to_hex uid] is [uid] as git prints it. *)

module Blk = Mgit_blk
module Sync = Mgit_sync
module Object = Mgit_object

module Make (Block : Blk.BLOCK) (Flow : Sync.S) : sig
  type mem
  (** The kind of an in-memory repository. *)

  type blk
  (** The kind of a repository on a block-device. *)

  type 'k t
  (** A repository of the kind ['k]. *)

  type 'k from =
    | Blk : Block.t -> blk from
    | Mem : mem from  (** A new, empty, in-memory repository. *)

  type remote

  val remote : Flow.ctx -> string -> (remote, [> `Msg of string ]) result
  (** [remote ctx url] is the remote repository [url], reached through the
      flows of [ctx]. [url] can be [git://host[:port]/path],
      [ssh://[user@]host[:port]/path], [[user@]host:path] or
      [http(s)://host[:port]/path], followed by [#branch] to choose the branch
      to follow ([main] by default). *)

  val format : ?ratio:float -> ?length:int -> Block.t -> (unit, [> error ]) result
  (** [format block] prepares [block] to hold a repository. [ratio] is the part
      of it used as a reception temporary buffer, the rest is split into the
      two zones of the generations. *)

  val make :
       ?branch:string
    -> ?depth:int
    -> ?now:(unit -> int)
    -> ?remote:remote
    -> 'k from
    -> ('k t, [> error ]) result
  (** [make from] opens a repository, with a handle on its default branch.
      - [branch] is the branch to follow when the repository is empty and
        when the [#branch] of the [remote] does not say it;
      - [depth] is the number of commits a generation keeps (1 by default),
        [0] keeps the whole history: nothing is shallow, and if the
        repository is (it was opened with another depth), the next
        {!val:pull} fetches what is behind its boundary (as
        [git fetch --unshallow]);
      - [now] gives the time (in seconds since the epoch) used to date the
        commits of the local changes;
      - [remote], if any, is where {!val:pull} fetches from and where the local
        changes are pushed to. Without it, the repository is purely local. *)

  val branch : 'k t -> string
  (** [branch t] is the branch of the handle [t] (like [refs/heads/main]). *)

  val branches : 'k t -> (string * uid) list
  (** [branches t] is the branches the repository follows and their commit,
      the default is first one. *)

  val checkout : 'k t -> string -> ('k t, [> error ]) result
  (** [checkout t name] is a handle on the branch [name] ([dev] or
      [refs/heads/dev]), [t] stays on its own (its branch does not change).
      When the repository does not follow [name] yet, it starts to: with the
      branch of the remote if it has one (fetched as {!val:pull} does), or with
      a new local branch at the commit of [t] otherwise (created on the remote
      by its first push). *)

  val forget : 'k t -> string -> (unit, [> error ]) result
  (** [forget t name] stops following the branch [name] (the commits only
      it needed disappear). The last branch can not be forgotten. A handle on
      [name] does not see anything anymore, and its changes are refused
      ([`Not_found]). *)

  val set_default : 'k t -> string -> (unit, [> error ]) result
  (** [set_default t name] makes the branch [name] (which we follow) the
      default one: the one of the handle {!val:make} gives, and the first
      reference of the bundle. It rewrites the whole generation: it is meant for rare
      changes. *)

  val commit : 'k t -> [ `Clean of uid | `Dirty of uid ] option
  (** [commit t] is the last commit of the branch of [t], [`Dirty] while we are inside of a
      {!val:change_and_push}, [None] for an empty repository. *)

  val get : 'k t -> string -> (perm * string, [> error ]) result
  (** [get t path] is the contents of the file [path] (e.g. ["/dir/file"]). *)

  val list : 'k t -> string -> ((string * [ `Value | `Dictionary ]) list, [> error ]) result
  (** [list t path] is the entries of the directory [path]. *)

  val fold : 'k t -> ('a -> path:string list -> perm -> string -> 'a) -> 'a -> 'a
  (** [fold t fn acc] folds over all the files of [t]. *)

  val set : 'k t -> ?perm:perm -> string -> string -> (unit, [> error ]) result
  (** [set t path contents] creates or replaces the file [path]. Missing
      directories are created. *)

  val remove : 'k t -> string -> (unit, [> error ]) result
  (** [remove t path] removes the file (or the directory) [path]. A directory
      which becomes empty is removed. *)

  val change_and_push :
       'k t
    -> ?author:string
    -> ?email:string
    -> ?message:string
    -> ('k t -> 'a)
    -> ('a, [> error ]) result
  (** [change_and_push t fn] applies all the changes [fn] makes as one single
      commit, and pushes it to the remote if there is one. A push which would
      not be a fast-forward (the remote has commits we do not have) is refused:
      you should {!val:pull} first.

      [fn] is given a handle on which {!val:set} and {!val:remove} are staged
      into the commit. From [fn], publishing is refused: a nested
      [change_and_push], a {!val:pull}, a {!val:gc}, and a change made through
      another handle than the given one. The given handle can not be used once
      [change_and_push] returned. *)

  val pull : 'k t -> ((string * change list) list, [> error ]) result
  (** [pull t] fetches the new commits of all the branches we follow from the
      remote, in one negotiation (only what we miss, negotiated as git does),
      and makes them the new generation. It returns, branch by branch, what
      changed between the previous generation and the new one. Local commits
      which were not pushed are dropped; a branch the remote does not have is
      kept as it is. *)

  val push : 'k t -> ?branches:string list -> remote -> (unit, [> error ]) result
  (** [push t ~branches remote] pushes to [remote] (another one than the
      remote of [t], e.g. a mirror) the [branches] ([dev] or
      [refs/heads/dev], all the branches we follow by default) as they are in
      the current generation, in one session. As for a change, a push which
      would not be a fast-forward is refused. *)

  val shallows : 'k t -> uid list
  (** [shallows t] is the commits we keep but whose parents we do not (the
      shallow boundary, as git writes it into [.git/shallow]). A server
      advertises them to its clients. *)

  val mem : 'k t -> uid -> bool
  (** [mem t uid] is [true] if the current generation has the object [uid]
      (e.g. to acknowledge a [have] of a client). *)

  val to_pack :
    'k t -> ?haves:uid list -> uid list -> (string -> unit) -> (unit, [> error ]) result
  (** [to_pack t ~haves wants fn] gives to [fn], chunk by chunk, the PACK file
      of what is reachable from the commits [wants] and not from the commits
      [haves] (the client has them). A [have] we do not know is ignored, a
      [want] we do not have (or which is not a commit) is an error. The PACK
      file is not thin. As {!val:to_bundle}, the generation is pinned while
      [fn] is called. *)

  type update =
    { name : string  (** The branch ([dev] or [refs/heads/dev]). *)
    ; old : uid option  (** What the client saw, [None] for a new branch. *)
    ; uid : uid  (** Its new commit. *) }

  val load :
       'k t
    -> updates:update list
    -> string Seq.t
    -> ((string * change list) list, [> error ]) result
  (** [load t ~updates pack] takes what a client pushes: the PACK file [pack]
      (which may be thin, or empty) and the branches it updates.

      Nothing changes if a branch is not at its [old] commit anymore, or if a
      new commit is not available. If [t] has a remote, the branches are pushed
      to this remote and if the remote refuses it, we don't publish/keep these
      objects. Otherweise, we create missing branches and move them into a new
      generation. [load], then, returns, branch by branch, what changed (as
      {!val:pull}). *)

  val gc : ?shallows:int -> 'k t -> (unit, [> error ]) result
  (** [gc ~shallows:n t] makes a new generation which only keeps the last [n]
      commits of each branch (the depth of [t] by default), and what they need.
      [n] becomes the depth of [t]. With [0], it keeps all the history it has
      (and the next {!val:pull} completes it). *)

  val to_bundle : 'k t -> (string -> unit) -> unit
  (** [to_bundle t fn] gives the current generation, as a Git bundle, chunk by
      chunk to [fn]. The generation is pinned while [fn] is called: [fn] must
      not publish twice (the second publication would wait for [fn]). *)
end
