(* ref.ml *)

(* This file introduces the type [HEAP] formalizing a heap
   as a mechanism for dynamically allocating memory cells.

   Two heap implementations are given:
   (1) [FCMBasedHeap], where references are implemented as first-class
       modules declaring effect names [Get] and [Set].
   (2) [RecordBasedHeap], where references are implemented as pairs of
       functions [get] and [set].
*)

open Effect
open Effect.Deep
open State


(* --------------------------------------------------------------------------- *)
(** Type Definitions. *)

(* [REF] is the interface of dynamically allocated references. *)
module type REF = sig
  type 'a t
  val ref  : 'a -> 'a t
  val (!)  : 'a t -> 'a
  val (:=) : 'a t -> 'a -> unit
  val run  : (unit -> 'a) -> 'a
end

(* [HEAP] is the type of a functor that, given the implementation of a cell,
   implements dynamically allocated references. *)
module type HEAP = CELL -> REF


(* --------------------------------------------------------------------------- *)
(** Heap Implementation Based on First-Class Modules. *)

(* [FCMBasedHeap] implements a heap using first-class modules.

   The idea is to implement the type of references ['a t] as the
   type of first-class modules declaring the pair of effect names
   [Get] and [Set].

   The operations [!] and [:=] are then simply implemented as [perform]
   instructions to one of the effect names passed as arguments.

   The interpretation of these operations is given by the functions
   [get] and [set] obtained from a new instance of [Cell].
*)

module FCMBasedHeap : HEAP = functor (Cell : CELL) -> struct
  (* [EFF] declares a pair of effect names [Get] and [Set]. *)
  module type EFF = sig
    type t
    type _ Effect.t += Get : t Effect.t
    type _ Effect.t += Set : t -> unit Effect.t
  end
  (* ['a t] is the type of first-class [EFF] modules.
     The effect-name declarations in [EFF] become first-class. *)
  type 'a t = (module EFF with type t = 'a)

  type _ Effect.t += Ref : 'a -> ('a t) Effect.t

  let ref init = perform (Ref init)
  let (!) : type a. a t -> a =
    fun (module E) -> perform E.Get
  let (:=) : type a. a t -> a -> unit =
    fun (module E) y -> perform (E.Set y)

  (* [fresh()] allocates fresh effect names [Get] and [Set],
      and packs these names into a first-class module. *)
  let fresh (type a) () : a t =
    (module struct
      type t = a
      type _ Effect.t += Get : t Effect.t
      type _ Effect.t += Set : t -> unit Effect.t
    end)

  let run main =
    try_with main () {
      effc = fun (type b) (e : b Effect.t) ->
        match e with
        | Ref init -> Some (fun (k : (b, _) continuation) ->
            (init, k) |> fun (type a) ((init, k) : a * (a t, _) continuation) ->
            let module E = (val (fresh() : a t)) in
            let module C = Cell(struct type t = a end) in
            let main() =
              try_with (continue k) (module E) {
                effc = fun (type c) (e : c Effect.t) ->
                  match e with
                  | E.Get -> Some (fun (k : (c, _) continuation) ->
                      continue k (C.get() : a))
                  | E.Set y -> Some (fun k ->
                      continue k (C.set y))
                  | _ -> None
              }
            in
            snd (C.run ~init main)
          )
        | _ -> None
    }
end


(* --------------------------------------------------------------------------- *)
(** Heap Implementation Based on Records. *)

(* [RecordBasedHeap] implements a reference as a pair of functions [get]
   and [set]. The operations [!] and [:=] need simply to choose between
   one these two functions. The operation [ref] is implemented as an
   effect [Ref]. When performed, a new instance of [Cell] is created and the
   continuation is resumed with the pair of functions [get] and [set] given
   by this new cell.
*)

module RecordBasedHeap : HEAP = functor (Cell : CELL) -> struct
  type 'a t = {
    get : unit -> 'a;
    set : 'a -> unit;
  }
  type _ Effect.t += Ref : 'a -> ('a t) Effect.t

  let ref init = perform (Ref init)
  let (!) {get; _} = get()
  let (:=) {set; _} y = set y

  let run main =
    try_with main () {
      effc = fun (type b) (e : b Effect.t) ->
        match e with
        | Ref init -> Some (fun (k : (b, _) continuation) ->
            (init, k) |> fun (type a) ((init, k) : a * (a t, _) continuation) ->
            let open Cell(struct type t = a end) in
            snd (run ~init (fun _ -> continue k {get; set}))
          )
        | _ -> None
    }
end


(* --------------------------------------------------------------------------- *)
(** Examples. *)

open Printf

let _ =
  printf "Opening module Ref...\n"

let _ =
  printf "Running tests...\n"

let _ =
  let heaps : (module REF) list = [
    (module FCMBasedHeap(StPassing));
    (module RecordBasedHeap(StPassing));
    (module FCMBasedHeap(LocalMutVar));
    (module RecordBasedHeap(LocalMutVar));
    (module FCMBasedHeap(GlobalMutVar));
    (module RecordBasedHeap(GlobalMutVar))
  ] in

  List.iter (fun heap ->
    let open (val heap : REF) in
    let main () =
      let fibs = ref [] in
      let a, b = ref 0, ref 1 in
      for _i = 0 to 10 do
        let fibsv, av, bv = !fibs, !a, !b in
        fibs := av :: fibsv;
        a := bv;
        b := av + bv
      done;
      let fibsv, av, bv = !fibs, !a, !b in
      assert (((List.hd fibsv), av, bv) = (55, 89, 144))
    in
    run main
  ) heaps

let _ =
  printf "End of tests.\n"

let _ =
  printf "End of module Ref.\n"
