module type S = sig
  type 'a t
  (** MVar type. Represents a data structure with a single hole that can be
      filled with value. *)

  val create : 'a -> 'a t
  (** [create v] allocates a new mvar with the hole filled with value [v]. *)

  val create_empty : unit -> 'a t
  (** [create_empty ()] allocates a new mvar with the hole empty. *)

  val put : 'a -> 'a t -> unit
  (** [put v m] fills mvar [m] with value v. If the mvar is already filled,
      this operation blocks until the hole become empty. *)

  val take : 'a t -> 'a
  (** [take m] empties the mvar [m] if it is filled and returns the value.
      If [m] is empty, then the operation blocks until the mvar becomes filled. *)
end

module type SCHED = sig
  type 'a cont
  (** Represents a blocked computation that waits for a value of type 'a. *)

  type _ eff += Suspend : ('a cont -> unit) -> 'a eff
  (** [perform @@ Suspend f] applies [f] to the current continuation, and suspends the
      execution of the current thread, and switches to the next thread in the
      scheduler's queue. *)

  type _ eff += Resume  : 'a cont * 'a -> unit eff
  (** [Perform @@ Resume (k,v)] prepares the suspended continuation [k] with value [v] and
      enqueues it to the scheduler queue. *)
end

module Make (S : SCHED) : S
