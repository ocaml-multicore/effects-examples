open Printf
open Effect
open Effect.Deep

module type STATE = sig
  type 'a t

  val ref  : 'a -> 'a t
  val (!)  : 'a t -> 'a
  val (:=) : 'a t -> 'a -> unit

  val run  : (unit -> 'a) -> 'a
end

module State : STATE = struct

  module type T = sig
    type elt
    type _ eff += Get : elt eff
    type _ eff += Set : elt -> unit eff
  end
  type 'a t = (module T with type elt = 'a)

  type _ eff += Ref : 'a -> 'a t eff
  let ref v = perform (Ref v)

  let (!)  : type a. a t -> a =
    fun (module R) -> perform R.Get

  let (:=) : type a. a t -> a -> unit =
    fun (module R) x -> perform (R.Set x)

  let run f =
    try_with f () {
      effc = fun e ->
        match e with 
        | Ref init -> Some (fun k ->
          (* trick to name the existential type introduced by the matching: *)
          (init, k) |> fun (type a) (init, k : a * (a t, _) continuation) ->
          let module R =
            struct
              type elt = a
              type _ eff += Get : elt eff
              type _ eff += Set : elt -> unit eff
            end
          in
          init |>
          match_with (continue k) (module R) {
            retc = (fun result -> fun _x -> result);
            exnc = raise;
            effc = fun e ->
              match e with
              | R.Get -> Some (fun (k : (a, a -> a t) continuation) -> fun x -> continue k x x)
              | R.Set y -> Some (fun k -> fun _x -> continue k () y)
              | _ -> None
          })
        | _ -> None
    }
    end

open State

let foo () =
  let r1 = ref "Hello" in
  let r2 = ref 10 in
  printf "%s\n" (!r1);
  printf "%d\n" (!r2);
  r1 := "World";
  r2 := 20;
  printf "%s\n" (!r1);
  printf "%d\n" (!r2);
  "Done"

let _ = run foo
