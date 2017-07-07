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
  let rec walk_tree : tree -> unit =
    function
    | Leaf e -> yield e
    | Node (l,r) -> walk_tree l; walk_tree r

  (* The comparator manages two walkers *)
  let comparator : tree -> tree -> bool
    = fun ltree rtree ->
      (* pointers to the continuation of the left and right trees,
         respectively *)
      let lk = ref (fun () -> walk_tree ltree; true) in
      let rk = ref (fun () -> walk_tree rtree; true) in
      let upd r k = r := (fun () -> continue k ()) in
      let rec loop () =
        match !lk ()  with
        | _ -> true
        | effect (Yield i) k ->
           match !rk () with
           | result -> result
           | effect (Yield j) k' ->
              if E.equals i j then
                let _ = upd lk k; upd rk k' in
                loop ()
              else
                false
      in
      loop ()

end

(* Instantiate SameFringe to work over integers *)
module SameFringe_Int = SameFringe(struct type t = int
                                          let equals x y = (Pervasives.compare x y) = 0
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

