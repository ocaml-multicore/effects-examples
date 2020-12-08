open Printf

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
    effect Get : elt
    effect Set : elt -> unit
  end
  type 'a t = (module T with type elt = 'a)

  effect Ref : 'a -> 'a t
  let ref v = perform (Ref v)

  let (!)  : type a. a t -> a =
    fun (module R) -> perform  R.Get

  let (:=) : type a. a t -> a -> unit =
    fun (module R) x -> perform (R.Set x)

  let run f =
    begin try
      f ()
    with
    | effect (Ref init) k ->
        (* trick to name the existential type introduced by the matching: *)
        (init, k) |> fun (type a) (init, k : a * (a t, _) continuation) ->
        let module R =
          struct
            type elt = a
            effect Get : elt
            effect Set : elt -> unit
          end
        in
        init |>
        begin match
          continue k (module R)
        with
        | result             -> fun _x -> result
        | effect  R.Get    k -> fun x -> continue k x  x
        | effect (R.Set y) k -> fun _x -> continue k () y
        end
    end
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
