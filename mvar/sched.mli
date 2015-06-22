type 'a cont
(** Represents a blocked computation that waits for a value of type 'a. *)

type _ eff += Suspend : ('a cont -> unit) -> 'a eff
(** [Perform @@ Suspend f] applies [f] to the current continuation, and suspends the
    execution of the current thread, and switches to the next thread in the
    scheduler's queue. *)

type _ eff += Resume : 'a cont * 'a -> unit eff
(** [perform @@ Resume (k,v)] prepares the suspended continuation [k] with value [v] and
    enqueues it to the scheduler queue. *)

type _ eff += Fork : (unit -> unit) -> unit eff
(** [perform @@ Fork f] forks [f] as a new thread to which control immediately switches to. *)

type _ eff += Yield : unit eff
(** [perform Yield] suspends the current thread and switches to the next thread from
    the run queue. *)

type _ eff += Get_Tid : int eff
(** [perform Get_Tid] returns the current thread identifier. *)

val run : (unit -> unit) -> unit
(** [run f] runs [f] with the cooperative-threaded scheduler. *)
