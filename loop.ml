open Effect
open Effect.Deep

type _ eff += Foo : (unit -> 'a) eff

let f () = perform Foo ()

let res : type a. a =
  match f () with
  | x -> x
  | effect Foo, k ->
     continue k (fun () -> perform Foo ())
