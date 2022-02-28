type 'a cont
(** Represents a blocked computation that waits for a value of type 'a. *)

type _ Effect.t += Suspend : ('a cont -> unit) -> 'a Effect.t
(** [Perform @@ Suspend f] applies [f] to the current continuation, and suspends the
    execution of the current thread, and switches to the next thread in the
    scheduler's queue. *)

type _ Effect.t += Resume : ('a cont * 'a) -> unit Effect.t
(** [perform @@ Resume (k,v)] prepares the suspended continuation [k] with value [v] and
    enqueues it to the scheduler queue. *)

type _ Effect.t += Fork : (unit -> unit) -> unit Effect.t
(** [perform @@ Fork f] forks [f] as a new thread to which control immediately switches to. *)

type _ Effect.t += Yield : unit Effect.t
(** [perform Yield] suspends the current thread and switches to the next thread from
    the run queue. *)

val run : (unit -> unit) -> unit
(** [run f] runs [f] with the cooperative-threaded scheduler. *)
