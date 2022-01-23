(* Old example from when there was a clone_continuation function *)

(* Why is `clone_continuation` in `Obj` module?
 * This is because one-shot continuations and multi-shot continuations mix
 * badly. This program illustrates that an resuming an inner one-shot
 * continuation within an outer multi-shot context causes runtime error.
 *)
open Effect
open Effect.Deep

type _ eff += Foo : unit eff
type _ eff += Bar : unit eff

let _ =
  let run () =
    try_with perform Foo {
      effc = fun (type a) (e : a eff) -> 
        match e with
        | Foo -> Some (fun (k : (a, _) continuation) -> continue k (perform Bar))  (* This continuation is resumed twice *)
        | _ -> None
    } 
  in
  try_with run () { 
    effc = fun (type a) (e : a eff) ->
      match e with
      | Bar -> Some (fun (k : (a, _) continuation) -> continue (Obj.clone_continuation k) (); continue k ())
      | _ -> None
  }
