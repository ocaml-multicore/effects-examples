open Printf

type bottom

module type TXN = sig
  type 'a t
  val atomically : (unit -> unit) -> unit
  val ref : 'a -> 'a t
  val (!) : 'a t -> 'a
  val (:=) : 'a t -> 'a -> unit
end

module Txn : TXN = struct
  type 'a t = 'a ref

  effect Update : 'a t * 'a -> unit

  let atomically f =
    let comp =
      match f () with
      | x -> (fun _ -> x)
      | exception e -> (fun rb -> rb (); raise e)
      | effect (Update (r,v)) k -> (fun rb ->
          let old_v = !r in
          r := v;
          continue k () (fun () -> r := old_v; rb ()))
    in comp (fun () -> ())

  let ref = ref
  let (!) = (!)
  let (:=) = fun r v -> perform (Update (r,v))
end

exception Res of int

open Txn

let () = atomically (fun () ->
  let r = ref 10 in
  printf "T0: %d\n" (!r);
  try atomically (fun () ->
    r := 20;
    r := 21;
    printf "T1: Before abort %d\n" (!r);
    raise (Res !r) |> ignore;
    printf "T1: After abort %d\n" (!r);
    r := 30)
  with
  | Res v -> printf "T0: T1 aborted with %d\n" v;
  printf "T0: %d\n" !r)
