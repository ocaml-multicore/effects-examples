(* This file contains a collection of attempts at replicating ML-style
   references using algebraic effects and handlers. The difficult thing
   to do is the dynamic creation of new reference cells at arbitrary
   types, without needing some kind of universal type or dynamic type
   checking. *)

module type Type = sig type t end
module Int = struct type t = int let compare = compare end

module LocalState (R : sig type t end) = struct
  type reff = R.t
  effect New : int -> R.t
  effect Get : R.t -> int
  effect Put : R.t * int -> unit
end

module type StateOps = sig
  type reff
  effect New : int -> reff
  effect Get : reff -> int
  effect Put : reff * int -> unit
end

(**********************************************************************)
(* version 1 : doesn't work, because declaration of new effect names
   is generative, so the handler and the client get different versions of
   the 'New', 'Get' and 'Put' effects. *)

let run main =
  let module S = LocalState (Int) in 
  let module IM = Map.Make (Int) in
  let comp =
    match main (module Int : Type) with
      | effect (S.New i) k ->
          fun s -> let r = fst (IM.max_binding s) + 1
                   in continue k r (IM.add r i s) 
      | effect (S.Get r) k ->
          fun s -> continue k (IM.find r s) s
      | effect (S.Put (r, i)) k ->
          fun s -> continue k () (IM.add r i s)
      | x -> fun s -> x
  in
  comp IM.empty

let main (module T : Type) =
  let module S = LocalState(T) in
  let x = perform (S.New 1) in
  perform (S.Put (x, 5));
  perform (S.Get x)

(**********************************************************************)
(* version 2 : working creation of freshly generated state cells, but
   only an int type. *)

let run2 main =
  let module S  = LocalState (Int) in
  let module IM = Map.Make (Int) in
  let comp =
    match main (module S : StateOps) with
      | effect (S.New i) k ->
          fun s ->
            let r = if IM.is_empty s then 0 else fst (IM.max_binding s) + 1
            in continue k r (IM.add r i s) 
      | effect (S.Get r) k ->
          fun s -> continue k (IM.find r s) s
      | effect (S.Put (r, i)) k ->
          fun s -> continue k () (IM.add r i s)
      | x -> fun s -> x
  in
  comp IM.empty

let main2 (module S : StateOps) =
  let open S in
  let x = perform (New 1) in
  perform (Put (x, 5));
  perform (Get x)

(**********************************************************************)
(* version 3, static creation of new state cells, requiring nested
   handlers. Similar to the example in "state.ml". *)
module type GetPutOps = sig
  type t
  effect Get : t
  effect Put : t -> unit
end

module MakeGetPut (T : sig type t end) () = struct
  type t = T.t
  effect Get : t
  effect Put : t -> unit
end

let run3 (type a) (module S : GetPutOps with type t = a) (s : a) main =
  let module IM = Map.Make (Int) in
  let comp =
    match main () with
      | effect S.Get k ->
          fun (s : S.t) -> continue k s s
      | effect (S.Put i) k ->
          fun s -> continue k () i
      | x -> fun s -> x
  in
  comp s

module S1 = MakeGetPut (struct type t = int end) ()
module S2 = MakeGetPut (struct type t = string end) ()

let test3 () =
  perform (S1.Put 5);
  let x = perform (S1.Get) in
  perform (S2.Put (string_of_int x ^ "xx"));
  perform S2.Get


(**********************************************************************)
(* version 4. Uses dynamic creation of new effect names to simulate
   the creation of new reference cells. Initially, there is only one
   effect 'New', which can be used to dynamically create new effect
   names. The handler for 'New' wraps the continuation in a new
   handler that handles the freshly generated effect names. This setup
   yields the same interface as ML refs, except that there is no way
   to compare references for equality. This is because cells are
   represeted as objects with a pair of a 'write' method and a 'read'
   method, so it is possible to create new references that reference
   the same underlying data without the access objects being
   equal. This is similar to the situation in Idealised Algol, where
   variables are ways to affect the state, but have no independent
   existence of their own.

   Compared to the example in "ref.ml", this implementation does not
   require a universal type, nor does it have "impossible" cases.

   This example also includes an unneccessary extra 'Choice' effect to
   demonstrate the combination of other effects with state in the same
   handler. This uses the experimental Obj.clone function to clone
   continuations. *)
type 'a reff = < get : 'a; put : 'a -> unit; internals : (module GetPutOps with type t = 'a) >

effect New : 'a -> 'a reff
effect Choice : bool

let run4 main =
  let donew : type a b. (a reff, b) continuation -> a -> b = fun k ->
      let module Ops = MakeGetPut (struct type t = a end) () in
      let cell = object
                   method get   = perform Ops.Get
                   method put x = perform (Ops.Put x)
                   method internals = (module Ops : GetPutOps with type t = a)
                 end
      in
      match continue k cell with
        | effect Ops.Get      k -> fun s -> continue k s s
        | effect (Ops.Put v)  k -> fun s -> continue k () v
        | x                     -> fun s -> x
  in
  match main () with
    | effect (New v) k -> donew k v
    | effect (Choice) k -> let k' = Obj.clone k in continue k true; continue k' false
    | x                -> x

let newref i = perform (New i)

let (:=) r x = r#put x

let (!) r = r#get

let test4 () =
  let a = newref 0 in
  let b = newref "str" in
  if perform Choice then
    begin a := String.length !b;
          b := string_of_int !a;
          print_endline !b
    end
  else
    print_endline !b
