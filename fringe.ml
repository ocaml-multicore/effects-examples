(* Same Fringe Problem

   Definition: Two binary trees have the same fringe if they have exactly
   the same leaves reading from left to right.

   Problem: Given two binary trees decide whether they have the same fringe.

   This problem can be elegantly solved using one-shot continuations.
*)

module type EQUATABLE = sig
  type t
  val equals : t -> t -> bool
end

(* Basic binary tree structure *)
type 'a tree =
  | Leaf of 'a
  | Node of 'a tree * 'a tree

module SameFringe(E : EQUATABLE) = struct

  type nonrec tree = E.t tree

  (* Yielding control *)
  effect Yield : E.t -> unit
  let yield e = perform (Yield e)

  (* The walk routine *)
  let rec walk : tree -> unit =
    function
    | Leaf e -> yield e
    | Node (l,r) -> walk l; walk r

  (* Reification of effects *)
  type resumption = (unit, step) continuation
  and step = Done
           | Yielded of E.t * resumption

  (* Reifies `Yield' effects *)
  let step f =
    match f () with
    | _ -> Done
    | effect (Yield e) k -> Yielded (e, k)

  (* The comparator "step walks" two given trees simultaneously *)
  let comparator ltree rtree =
    let l = fun () -> step (fun () -> walk ltree) in
    let r = fun () -> step (fun () -> walk rtree) in
    let rec stepper l r =
      (* There are three cases to consider:
         1) Both walk routines are done in which case the trees must have
         the same fringe.
         2) Both walk routines have yielded a value. There are two
            subcases to consider:
              a) the values are equal in which case the walk routines
                 are continued
              b) the values differ which implies that the trees do not have
                 the same fringe.
         3) Either walk routine is done, while the other yielded,
         which implies the one tree has a larger fringe than the other.  *)
      match l (), r () with
      | Done, Done -> true
      | Yielded (e, k), Yielded (e', k') ->
         if E.equals e e'
         then stepper (fun () -> continue k ()) (fun () -> continue k' ())
         else false
      | _, _ -> false
    in
    stepper l r

end

(* Instantiate SameFringe to work over integers *)
module SameFringe_Int =
  SameFringe(struct type t = int
                    let equals x y = (Stdlib.compare x y) = 0
             end)

(* Some examples *)
let ex1 = Node (Leaf 1, Node (Leaf 2, Leaf 3))
let ex2 = Node (Node (Leaf 1, Leaf 2), Leaf 3)
let ex3 = Node (Node (Leaf 3, Leaf 2), Leaf 1)

let ex4 = Leaf 42
let ex5 = Leaf 41

let _ =
  let open SameFringe_Int in
  let pairs = [ex1,ex2; ex2,ex1; ex1,ex3; ex3,ex2; ex4,ex4; ex5,ex4] in
  List.iter
    (function
    | true -> print_endline "same"
    | false -> print_endline "different")
    (List.map (fun (l,r) -> comparator l r) pairs);
  flush stdout

