(* This file contains a collection of attempts at replicating ML-style
   references using algebraic effects and handlers. The difficult thing
   to do is the dynamic creation of new reference cells at arbitrary
   types, without needing some kind of universal type or dynamic type
   checking. *)

open Effect
open Effect.Deep

module type Type = sig
  type t
end

module Int = struct
  type t = int

  let compare = compare
end

module LocalState (R : sig
  type t
end) =
struct
  type reff = R.t
  type _ eff += New : int -> R.t eff
  type _ eff += Get : R.t -> int eff
  type _ eff += Put : R.t * int -> unit eff
end

module type StateOps = sig
  type reff
  type _ eff += New : int -> reff eff
  type _ eff += Get : reff -> int eff
  type _ eff += Put : reff * int -> unit eff
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
    | x -> fun _s -> x
    | effect (S.Put (r, i)), k ->
        fun s -> continue k () (IM.add r i s)
    | effect (S.Get r), k ->
        fun s -> continue k (IM.find r s) s
    | effect (S.New i), k ->
        fun s ->
          let r = fst (IM.max_binding s) + 1 in
          continue k r (IM.add r i s)
  in
  comp IM.empty

let main (module T : Type) =
  let module S = LocalState (T) in
  let x = perform (S.New 1) in
  perform (S.Put (x, 5));
  perform (S.Get x)

(**********************************************************************)
(* version 2 : working creation of freshly generated state cells, but
   only an int type. *)
let run2 main =
  let module S = LocalState (Int) in
  let module IM = Map.Make (Int) in
  let comp =
    match main (module S : StateOps) with
    | x -> fun _s -> x
    | effect (S.Put (r, i)), k ->
        fun s -> continue k () (IM.add r i s)
    | effect (S.Get r), k ->
        fun s -> continue k (IM.find r s) s
    | effect (S.New i), k ->
        fun s ->
          let r =
            if IM.is_empty s then 0 else fst (IM.max_binding s) + 1
          in
          continue k r (IM.add r i s)
  in
  comp IM.empty

let main2 (module S : StateOps) =
  let open S in
  let x = perform (New 1) in
  perform (Put (x, 5));
  perform (Get x)

(**********************************************************************)
(* version 3, static creation of new state cells, requiring nested
   handlers. Similar to the example in "state.ml".
*)
module type GetPutOps = sig
  type t
  type _ eff += Get : t eff
  type _ eff += Put : t -> unit eff
end

module MakeGetPut
    (T : sig
      type t
    end)
    () =
struct
  type t = T.t
  type _ eff += Get : t eff
  type _ eff += Put : t -> unit eff
end

let run3 (type a) (module S : GetPutOps with type t = a) (s : a) main =
  let module IM = Map.Make (Int) in
  let comp =
    match main () with
    | x -> fun _s -> x
    | effect S.Get, k -> fun (s : S.t) -> continue k s s
    | effect (S.Put i), k -> fun _s -> continue k () i
  in
  comp s

module S1 =
  MakeGetPut
    (struct
      type t = int
    end)
    ()

module S2 =
  MakeGetPut
    (struct
      type t = string
    end)
    ()

let test3 () : string =
  perform (S1.Put 5);
  let x = perform S1.Get in
  perform (S2.Put (string_of_int x ^ "xx"));
  perform S2.Get

(* NOTE we can run with string state inside the integer state.
   or swap around the state and have integer state inside the string state.
   Both work!
*)

let main3 () = run3 (module S1) 0 (fun () -> run3 (module S2) "" test3)
let main3' () = run3 (module S2) "" (fun () -> run3 (module S1) 0 test3)

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
   handler. This uses the external `Multicont.Deep.clone_continuation`
   function to clone continuations. *)
type 'a reff =
  < get : 'a
  ; put : 'a -> unit
  ; internals : (module GetPutOps with type t = 'a) >

type _ eff += New : 'a -> 'a reff eff
type _ eff += Choice : bool eff

let run4 main =
  let donew : type a b. (a reff, b) continuation -> a -> b =
   fun k ->
    let module Ops =
      MakeGetPut
        (struct
          type t = a
        end)
        ()
    in
    let cell =
      object
        method get = perform Ops.Get
        method put x = perform (Ops.Put x)
        method internals = (module Ops : GetPutOps with type t = a)
      end
    in
    (match continue k cell with
     | x -> fun (_s : a) -> x
     | effect (Ops.Put v), k -> fun (_s : a) -> continue k () v
     | effect Ops.Get, k -> fun (s : a) -> continue k s s)
  in
  match main () with
  | x -> x
  | effect (New v), k -> donew k v
  | effect Choice, k ->
      let k' = Multicont.Deep.clone_continuation k in
      continue k true;
      continue k' false

let newref i = perform (New i)
let ( := ) r x = r#put x
let ( ! ) r = r#get

let test4 () =
  let a = newref 0 in
  let b = newref "str" in
  if perform Choice then (
    a := String.length !b;
    b := string_of_int !a;
    print_endline !b)
  else print_endline !b

let main4 () = run4 test4
