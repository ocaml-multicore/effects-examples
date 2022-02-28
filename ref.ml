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
    type _ Effect.t += Get : elt Effect.t
    type _ Effect.t += Set : elt -> unit Effect.t
  end
  type 'a t = (module T with type elt = 'a)

  type _ Effect.t += Ref : 'a -> 'a t Effect.t
  let ref v = perform (Ref v)

  let (!)  : type a. a t -> a =
    fun (module R) -> perform R.Get

  let (:=) : type a. a t -> a -> unit =
    fun (module R) x -> perform (R.Set x)

  let run f =
    try_with f () {
      effc = fun (type a) (e : a Effect.t) ->
        match e with 
        | Ref init -> Some (fun (k : (a, _) continuation) ->
          (* trick to name the existential type introduced by the matching: *)
          (init, k) |> fun (type b) (init, k : b * (b t, _) continuation) ->
          let module R =
            struct
              type elt = b
              type _ Effect.t += Get : elt Effect.t
              type _ Effect.t += Set : elt -> unit Effect.t
            end
          in
          init |>
          match_with (continue k) (module R) {
            retc = (fun result -> fun _x -> result);
            exnc = raise;
            effc = fun (type c) (e : c Effect.t) ->
              match e with
              | R.Get -> Some (fun (k : (c, _) continuation) -> fun (x : b) -> continue k x x)
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
