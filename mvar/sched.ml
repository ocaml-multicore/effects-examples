open Effect
open Effect.Deep

type _ eff += Fork : (unit -> unit) -> unit eff
type _ eff += Yield : unit eff
type 'a cont = ('a, unit) continuation
type _ eff += Suspend : ('a cont -> unit) -> 'a eff
type _ eff += Resume : 'a cont * 'a -> unit eff

let run main =
  let run_q = Queue.create () in
  let enqueue t v = Queue.push (fun () -> continue t v) run_q in
  let dequeue () = if Queue.is_empty run_q then () else Queue.pop run_q () in
  let rec spawn f =
    match f () with
    | () -> dequeue ()
    | effect Yield, k ->
        enqueue k ();
        dequeue ()
    | effect (Fork f), k ->
        enqueue k ();
        spawn f
    | effect (Suspend f), k ->
        f k;
        dequeue ()
    | effect (Resume (k', v)), k ->
        enqueue k' v;
        ignore (continue k ())
  in
  spawn main

let fork f = perform (Fork f)
let yield () = perform Yield
let suspend f = perform (Suspend f)
let resume (k, v) = perform (Resume (k, v))
