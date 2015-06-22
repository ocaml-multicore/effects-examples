module type S = sig
  type 'a t
  val new_mvar       : 'a -> 'a t
  val new_empty_mvar : unit -> 'a t
  val put_mvar       : 'a -> 'a t -> unit
  val take_mvar      : 'a t -> 'a
end

module type SCHED = sig
  type 'a cont
  effect Suspend : ('a cont -> unit) -> 'a
  effect Resume  : 'a cont * 'a -> unit
end

module Make (S : SCHED) : S = struct

  (** The state of mvar is either [Full v q] filled with value [v] and a queue
      [q] of threads waiting to fill the mvar, or [Empty q], with a queue [q] of
      threads waiting to empty the mvar. *)
  type 'a mv_state =
    | Full  of 'a * ('a * unit S.cont) Queue.t
    | Empty of 'a S.cont Queue.t

  type 'a t = 'a mv_state ref

  let new_empty_mvar () = ref (Empty (Queue.create ()))

  let new_mvar v = ref (Full (v, Queue.create ()))

  let suspend f = perform @@ S.Suspend f
  let resume (a,b) = perform @@ S.Resume (a,b)

  let put_mvar v mv =
    match !mv with
    | Full (v', q) -> suspend (fun k -> Queue.push (v,k) q)
    | Empty q ->
        if Queue.is_empty q then
          mv := Full (v, Queue.create ())
        else
          let t = Queue.pop q in
          resume (t, v)

  let take_mvar mv =
    match !mv with
    | Empty q -> suspend (fun k -> Queue.push k q)
    | Full (v, q) ->
        if Queue.is_empty q then
          (mv := Empty (Queue.create ()); v)
        else
          let (v', t) = Queue.pop q in
          (mv := Full (v', q);
           resume (t, ());
           v)
end
