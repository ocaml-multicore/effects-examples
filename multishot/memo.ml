open Effect
open Effect.Deep

(* OCaml 5 removed the ability to clone continuations. See this conversation
   https://discuss.ocaml.org/t/multi-shot-continuations-gone-forever/9072.

   It is possible this example could be revived using:
   https://github.com/dhil/ocaml-multicont
*)

module Memo : sig

  val memoize : ('a -> 'b) -> ('a -> 'b)
  (** [memoize f] returns the memoized version of [f] that caches the
   * evaluation of [f] from the start of [f] to the last invocation of [cut ()]
   * in [f], with respect to some input [x] to the memoized function.
   * Subsequent invocations of the memoized function with the same input [x]
   * only evaluates the continuation of the [cut ()].
   *
   * If the memoized function is applied to [y], where [not (x = y)], the memo
   * cache is updated.
   *)

  val cut : unit -> unit
  (** [cut ()] marks the end of memoization. If a memoized function has
   * multiple [cut()], the function is memoized until the last cut. Invoking a
   * memoized function without establishing a cut is an error.
   *)

end = struct

  type _ Effect.t += Cut : unit Effect.t
  let cut () = perform Cut

  type ('a,'b) cache_entry =
    {input : 'a;
     mutable cont : unit -> 'b}

  let memoize f =
    let cache = ref None in
    fun x ->
      try_with
        (fun () -> match !cache with
        | Some {input; cont} when x = input -> cont ()
        | _ ->
            let err_msg = "Memoized function was not cut" in
            cache := Some {input = x; cont = fun () -> failwith err_msg};
            f x) 
        ()
        { effc = fun (type a) (e : a Effect.t) ->
          match e with
          | Cut -> Some (fun (k : (a, _) continuation) ->
            match !cache with
            | Some c ->
                let rec save_cont k () =
                  c.cont <- save_cont (Multicont.Deep.clone_continuation k);
                  continue k ()
                in
                save_cont k ()
            | None -> failwith "impossible")
          | _ -> None}
end

let print_succ x =
  Printf.printf "input change: %d\n" x;
  (* ......
   * expensive computation
   * .....*)
  Memo.cut();
  Printf.printf "Succ of %d is %d\n" x (x+1)

let memoized_print_succ = Memo.memoize print_succ

let test () =
  memoized_print_succ 0;
  memoized_print_succ 0;
  memoized_print_succ 0;
  memoized_print_succ 1;
  memoized_print_succ 1;
  memoized_print_succ 1

let _ = test ()
