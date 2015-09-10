type 'a t
val create : 'a -> 'a t
val create_empty : unit -> 'a t
val put : 'a t -> 'a -> unit Sched_monad.t
val take : 'a t -> 'a Sched_monad.t
