(* Reverse-mode Algorithmic differentiation using effect handlers.
   Adapted from https://twitter.com/tiarkrompf/status/963314799521222656.
   See https://openreview.net/forum?id=SJxJtYkPG for more information. *)
open Effect
open Effect.Deep

module F : sig
  type t

  val mk : float -> t
  val ( +. ) : t -> t -> t
  val ( *. ) : t -> t -> t
  val grad : (t -> t) -> float -> float
  val grad2 : (t * t -> t) -> float * float -> float * float
end = struct
  type t = { v : float; mutable d : float }

  let mk v = { v; d = 0.0 }

  type _ eff += Add : t * t -> t eff
  type _ eff += Mult : t * t -> t eff

  let run f =
    ignore (match f () with
      | r -> r.d <- 1.0; r;
      | effect (Add(a,b)), k ->
          let x = {v = a.v +. b.v; d = 0.0} in
          ignore (continue k x);
          a.d <- a.d +. x.d;
          b.d <- b.d +. x.d;
          x
      | effect (Mult(a,b)), k ->
          let x = {v = a.v *. b.v; d = 0.0} in
          ignore (continue k x);
          a.d <- a.d +. (b.v *. x.d);
          b.d <- b.d +. (a.v *. x.d);
          x)

  let grad f x =
    let x = mk x in
    run (fun () -> f x);
    x.d

  let grad2 f (x, y) =
    let x, y = (mk x, mk y) in
    run (fun () -> f (x, y));
    (x.d, y.d)

  let ( +. ) a b = perform (Add (a, b))
  let ( *. ) a b = perform (Mult (a, b))
end
;;

(* f = x + x^3 =>
   df/dx = 1 + 3 * x^2 *)
for x = 0 to 10 do
  let x = float_of_int x in
  assert (F.(grad (fun x -> x +. (x *. x *. x)) x) = 1.0 +. (3.0 *. x *. x))
done
;;

(* f = x^2 + x^3 =>
   df/dx = 2*x + 3 * x^2 *)
for x = 0 to 10 do
  let x = float_of_int x in
  assert (
    F.(grad (fun x -> (x *. x) +. (x *. x *. x)) x)
    = (2.0 *. x) +. (3.0 *. x *. x))
done
;;

(* f = x^2 * y^4 =>
   df/dx = 2 * x * y^4
   df/dy = 4 * x^2 * y^3 *)
for x = 0 to 10 do
  for y = 0 to 10 do
    let x = float_of_int x in
    let y = float_of_int y in
    assert (
      F.(grad2 (fun (x, y) -> x *. x *. y *. y *. y *. y) (x, y))
      = (2.0 *. x *. y *. y *. y *. y, 4.0 *. x *. x *. y *. y *. y))
  done
done
