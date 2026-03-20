(* Multi-shot continuations don't play nicely with linear resources.
 * This program illustrates that resuming an inner one-shot continuation
 * within an outer multi-shot context causes a runtime error.
 *)
open Effect
open Effect.Deep

type _ eff += Foo : unit eff
type _ eff += Bar : unit eff

let _ =
  let run () =
    match perform Foo with
    | x -> x
    | effect Foo, k -> continue k (perform Bar)
  in
  match run () with
  | () -> ()
  | effect Bar, k ->
      continue (Multicont.Deep.clone_continuation k) ();
      continue k ()
