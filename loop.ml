open Effect
open Effect.Deep

type _ Effect.t += Foo : (unit -> 'a) Effect.t

let f () = perform Foo ()

let res : type a. a =
  try_with f ()
    {
      effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | Foo ->
              Some
                (fun (k : (a, _) continuation) ->
                  continue k (fun () -> perform Foo ()))
          | _ -> None);
    }
