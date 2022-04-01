(* Example in the delimcc paper:
 * http://okmij.org/ftp/continuations/caml-shift-journal.pdf *)

open Delimcc.M;;

(* A finite map: a search tree *)
type ('k, 'v) tree =
  | Empty
  | Node of ('k, 'v) tree * 'k * 'v * ('k, 'v) tree
;;

exception NotFound
;;

(* Update the value associated with the key k by applying the
   update function f. Return the new tree.
   If the key is not found, throw an exception.
*)
let rec update1 : 'k -> ('v->'v) -> ('k,'v) tree -> ('k,'v) tree =
  fun k f ->
    let rec loop = function
    | Empty -> raise NotFound
    | Node (l,k1,v1,r) ->
	begin
	  match compare k k1 with
	  | 0 -> Node(l,k1,f v1,r)
	  | n when n < 0 -> Node(loop l,k1,v1,r)
	  | _            -> Node(l,k1,v1,loop r)
	end
    in loop
;;

(* Add to the tree the association of the key k to the value v,
   overriding any existing association with the key k, if any.
*)
let rec insert k v = function
  | Empty -> Node(Empty,k,v,Empty)
  | Node(l,k1,v1,r) ->
      begin
	match compare k k1 with
	| 0 -> Node(l,k1,v,r)
	| n when n < 0 -> Node(insert k v l, k1,v1,r)
	| _            -> Node(l,k1,v1,insert k v r)
      end
;;

(* A re-balancing function; dummy for now *)
let rebalance : ('k,'v) tree -> ('k,'v) tree =
  fun t ->
    print_endline "Rebalancing"; t
;;

(* Examples of using update1 *)
let tree1 =
 let n1 = Node(Empty,1,101,Empty) in
 let n9 = Node(Empty,9,109,Empty) in
 let n5 = Node(n1,5,105,Empty) in
 let n7 = Node(n5,7,107,n9) in
 n7;;

let Node (Node (Node (Empty, 1, 102, Empty), 5, 105, Empty), 7, 107,
	Node (Empty, 9, 109, Empty))
    =
  try update1 1 succ tree1
  with NotFound -> insert 1 100 tree1
;;


let
  Node
   (Node (Node (Node (Empty, 0, 100, Empty), 1, 101, Empty), 5, 105, Empty),
   7, 107, Node (Empty, 9, 109, Empty))
 =
  try update1 0 succ tree1
  with NotFound -> insert 0 100 tree1
;;


(* The same as update1, but using Delimcc *)

let rec update2 : ('k,'v) tree option prompt ->
  'k -> ('v->'v) -> ('k,'v) tree -> ('k,'v) tree =
  fun pnf k f ->
    let rec loop = function
    | Empty -> abort pnf None
    | Node (l,k1,v1,r) ->
	begin
	  match compare k k1 with
	  | 0 -> Node(l,k1,f v1,r)
	  | n when n < 0 -> Node(loop l,k1,v1,r)
	  | _            -> Node(l,k1,v1,loop r)
	end
    in loop
;;

let Node (Node (Node (Empty, 1, 102, Empty), 5, 105, Empty), 7, 107,
	Node (Empty, 9, 109, Empty))
    =
  let pnf = new_prompt () in
  match push_prompt pnf (fun () ->
    Some (update2 pnf 1 succ tree1)) with
  | Some tree -> tree
  | None      -> insert 1 100 tree1
;;

let
  Node
   (Node (Node (Node (Empty, 0, 100, Empty), 1, 101, Empty), 5, 105, Empty),
   7, 107, Node (Empty, 9, 109, Empty))
 =
  let pnf = new_prompt () in
  match push_prompt pnf (fun () ->
    Some (update2 pnf 0 succ tree1)) with
  | Some tree -> tree
  | None      -> insert 0 100 tree1
;;

(* Resumable exceptions *)
(* upd_handle is very problematic! *)
let upd_handle = fun k -> raise NotFound;;
let rec update3 : 'k -> ('v->'v) -> ('k,'v) tree -> ('k,'v) tree =
  fun k f ->
    let rec loop = function
    | Empty -> Node(Empty,k,upd_handle k,Empty)
    | Node (l,k1,v1,r) ->
	begin
	  match compare k k1 with
	  | 0 -> Node(l,k1,f v1,r)
	  | n when n < 0 -> Node(loop l,k1,v1,r)
	  | _            -> Node(l,k1,v1,loop r)
	end
    in loop
;;

let Node (Node (Node (Empty, 1, 102, Empty), 5, 105, Empty), 7, 107,
	Node (Empty, 9, 109, Empty))
    =
  update3 1 succ tree1
;;

(* Resumable exceptions *)

type ('k,'v) res = Done of ('k,'v) tree
  | ReqNF of 'k * ('v,('k,'v) res) subcont
;;
let rec update4 : ('k,'v) res prompt ->
  'k -> ('v->'v) -> ('k,'v) tree -> ('k,'v) tree =
  fun pnf k f ->
    let rec loop = function
    | Empty -> Node(Empty,k,take_subcont pnf (fun c -> ReqNF (k,c)),Empty)
    | Node (l,k1,v1,r) ->
	begin
	  match compare k k1 with
	  | 0 -> Node(l,k1,f v1,r)
	  | n when n < 0 -> Node(loop l,k1,v1,r)
	  | _            -> Node(l,k1,v1,loop r)
	end
    in loop
;;


let Node (Node (Node (Empty, 1, 102, Empty), 5, 105, Empty), 7, 107,
	Node (Empty, 9, 109, Empty))
    =
  let pnf = new_prompt () in
  match push_prompt pnf (fun () ->
    Done (update4 pnf 1 succ tree1)) with
  | Done tree    -> tree
  | ReqNF (k,c)  ->
      rebalance (match push_subcont c 100 with Done x -> x)
;;

let
  Node
   (Node (Node (Node (Empty, 0, 100, Empty), 1, 101, Empty), 5, 105, Empty),
   7, 107, Node (Empty, 9, 109, Empty))
 =
  let pnf = new_prompt () in
  match push_prompt pnf (fun () ->
    Done (update4 pnf 0 succ tree1)) with
  | Done tree    -> tree
  | ReqNF (k,c)  ->
      rebalance (match push_subcont c 100 with Done x -> x)
;;
(* Rebalancing is printed *)



(* A custom value update function *)
exception TooBig;;

let upd_fun n = if n > 5 then raise TooBig else succ n;;

(* Several exceptions *)
let Empty =
 try
  try update1 7 upd_fun tree1
  with NotFound -> insert 7 100 tree1
 with TooBig -> Empty
;;


let Empty =
 try
  let pnf = new_prompt () in
  match push_prompt pnf (fun () ->
    Done (update4 pnf 7 upd_fun tree1)) with
  | Done tree    -> tree
  | ReqNF (k,c)  ->
      rebalance (match push_subcont c 100 with Done x -> x)
 with TooBig -> Empty
;;
