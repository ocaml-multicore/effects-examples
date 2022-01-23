open Effect
open Effect.Deep

type _ eff += Foo : (unit -> 'a) eff

let f () = perform Foo ()

let res : type a. a =
  try_with f () {
    effc = fun (type a) (e : a eff) ->
      match e with
      | Foo -> Some (fun (k : (a, _) continuation) -> continue k (fun () -> perform Foo ()))
      | _ -> None
  }
