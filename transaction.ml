open Printf

type bottom = ..

module type TXN = sig
  type 'a t
  val atomically : (unit -> unit) -> unit
  val ref : 'a -> 'a t
  val (!) : 'a t -> 'a
  val (:=) : 'a t -> 'a -> unit
  val abort : exn -> bottom
end

module Txn : TXN = struct
  type 'a t = 'a ref

  effect Update : 'a t * 'a -> unit
  effect Abort : exn -> bottom

  let ref = ref

  let atomically f =
    let comp =
      match f () with
      | x -> (fun _ -> x)
      | effect (Abort x) k -> (fun rb -> rb (); raise x)
      | effect (Update (r,v)) k -> (fun rb ->
          let old_v = !r in
          r := v;
          continue k () (fun () -> r := old_v; rb ()))
    in comp (fun () -> ())

  let (!) = (!)
  let (:=) = fun r v -> perform (Update (r,v))
  let abort v = perform (Abort v)
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
    ignore (abort (Res !r));
    printf "T1: After abort %d\n" (!r);
    r := 30)
  with
  | Res v -> printf "T0: T1 aborted with %d\n" v;
  printf "T0: %d\n" !r)
