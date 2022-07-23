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

  type 'a t = {
    get : unit -> 'a;
    set : 'a -> unit;
  }

  type _ Effect.t += Ref : 'a -> 'a t Effect.t
  let ref init = perform (Ref init)

  let (!)  : type a. a t -> a =
    fun {get; _} -> get ()

  let (:=) : type a. a t -> a -> unit =
    fun {set; _} y -> set y

  let run f =
    try_with f () {
      effc = fun (type a) (e : a Effect.t) ->
        match e with 
        | Ref init -> Some (fun (k : (a, _) continuation) ->
          (* trick to name the existential type introduced by the matching: *)
          (init, k) |> fun (type b) (init, k : b * (b t, _) continuation) ->
          let open struct
            type _ Effect.t += Get : b Effect.t
            type _ Effect.t += Set : b -> unit Effect.t
          end in
          let get () = perform Get in
          let set y = perform (Set y) in
          init |>
          match_with (continue k) {get; set} {
            retc = (fun result -> fun _x -> result);
            exnc = raise;
            effc = fun (type c) (e : c Effect.t) ->
              match e with
              | Get -> Some (fun (k : (c, _) continuation) -> fun (x : b) -> continue k x x)
              | Set y -> Some (fun k -> fun _x -> continue k () y)
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
