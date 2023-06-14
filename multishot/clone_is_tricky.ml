(* Multi-shot continuations don't play nicely with linear resources.
 * This program illustrates that resuming an inner one-shot continuation 
 * within an outer multi-shot context causes a runtime error.
 *)
open Effect
open Effect.Deep

type _ Effect.t += Foo : unit Effect.t
type _ Effect.t += Bar : unit Effect.t

let _ =
  let run () =
    try_with perform Foo {
      effc = fun (type a) (e : a Effect.t) -> 
        match e with
        | Foo -> Some (fun (k : (a, _) continuation) -> continue k (perform Bar))  (* This continuation is resumed twice *)
        | _ -> None
    } 
  in
  try_with run () { 
    effc = fun (type a) (e : a Effect.t) ->
      match e with
      | Bar -> Some (fun (k : (a, _) continuation) -> continue (Multicont.Deep.clone_continuation k) (); continue k ())
      | _ -> None
  }
