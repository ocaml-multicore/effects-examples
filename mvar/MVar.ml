module type S = sig
  type 'a t

  val create : 'a -> 'a t
  val create_empty : unit -> 'a t
  val put : 'a -> 'a t -> unit
  val take : 'a t -> 'a
end

module type SCHED = sig
  type 'a cont
  type _ eff += Suspend : ('a cont -> unit) -> 'a eff
  type _ eff += Resume : 'a cont * 'a -> unit eff
end

module Make (S : SCHED) : S = struct
  open Effect

  (** The state of mvar is either [Full v q] filled with value [v] and a queue
      [q] of threads waiting to fill the mvar, or [Empty q], with a queue [q] of
      threads waiting to empty the mvar. *)
  type 'a mv_state =
    | Full of 'a * ('a * unit S.cont) Queue.t
    | Empty of 'a S.cont Queue.t

  type 'a t = 'a mv_state ref

  let create_empty () = ref (Empty (Queue.create ()))
  let create v = ref (Full (v, Queue.create ()))
  let suspend f = perform @@ S.Suspend f
  let resume (a, b) = perform @@ S.Resume (a, b)

  let put v mv =
    match !mv with
    | Full (v', q) -> suspend (fun k -> Queue.push (v, k) q)
    | Empty q ->
        if Queue.is_empty q then mv := Full (v, Queue.create ())
        else
          let t = Queue.pop q in
          resume (t, v)

  let take mv =
    match !mv with
    | Empty q -> suspend (fun k -> Queue.push k q)
    | Full (v, q) ->
        if Queue.is_empty q then (
          mv := Empty (Queue.create ());
          v)
        else
          let v', t = Queue.pop q in
          mv := Full (v', q);
          resume (t, ());
          v
end
