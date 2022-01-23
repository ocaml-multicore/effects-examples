(* This example is intended to show that performing an effect in a C Callback,
 * whose handler is outside the current callback isn't sensible. This
 * corresponds to the stack given below (stack grows downward):
 *
 * +-----------------+
 * |       main      |
 * | (try .... with) | //OCaml frame
 * +-----------------+
 * |    caml_to_c    | //C frame -- OCaml calls to C
 * +-----------------+
 * |    c_to_caml    |
 * |   (perform e)   | //OCaml frame -- C callback to OCaml
 * +-----------------+
 *
 * This doesn't work because of the fact that there are intervening C frames
 * which cannot be captured as a part of the continuation. Expected output is:
 *
 * [Caml] Call caml_to_c
 * [C] Enter caml_to_c
 * [C] Call c_to_caml
 * [Caml] Enter c_to_caml
 * Fatal error: exception Unhandled
 *)
open Effect
open Effect.Deep

type _ eff += E : unit eff

let printf = Printf.printf

let c_to_caml () =
  printf "[Caml] Enter c_to_caml\n%!";
  perform E;
  printf "[Caml] Leave c_to_caml\n%!"

let _ = Callback.register "c_to_caml" c_to_caml

external caml_to_c : unit -> unit = "caml_to_c"

let _ =
  let f () =
    printf "[Caml] Call caml_to_c\n%!";
    caml_to_c ();
    printf "[Caml] Return from caml_to_c\n%!"
  in
  try_with f () {
    effc = fun (type a) (e : a eff) ->
      match e with
      | E -> Some (fun (k : (a, _) continuation) ->
        printf "[Caml] Handle effect E. Continuing..\n%!";
        continue k ())
      | _ -> None
  }

