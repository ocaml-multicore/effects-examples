open Effect
open Effect.Deep

type _ eff += Fork : (unit -> unit) -> unit eff
type _ eff += Yield : unit eff

type 'a cont = ('a,unit) continuation
type _ eff += Suspend : ('a cont -> unit) -> 'a eff
type _ eff += Resume : 'a cont * 'a -> unit eff

let run main =
  let run_q = Queue.create () in
  let enqueue t v =
    Queue.push (fun () -> continue t v) run_q
  in
  let dequeue () =
    if Queue.is_empty run_q then ()
    else Queue.pop run_q ()
  in
  let rec spawn f =
    match_with f () {
      retc = dequeue;
      exnc = raise;
      effc = fun (type a) (e : a Effect.t) ->
        match e with
        | Yield -> Some (fun (k : (a, _) continuation) -> enqueue k (); dequeue ())
        | Fork f -> Some (fun k -> enqueue k (); spawn f)
        | Suspend f -> Some (fun k -> f k; dequeue ())
        | Resume (k', v) -> Some (fun k ->
            enqueue k' v; ignore (continue k ()))
        | _ -> None
    }
  in
  spawn main

let fork f = perform (Fork f)
let yield () = perform Yield
let suspend f = perform (Suspend f)
let resume (k,v) = perform (Resume (k,v))
