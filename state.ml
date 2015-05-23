open Printf

module type STATE = sig
  type t
  val put : t -> unit
  val get : unit -> t
  val run : (unit -> unit) -> init:t -> unit
end

module State (S : sig type t end) : STATE with type t = S.t = struct

  type t = S.t

  effect Put : t -> unit
  let put v = perform (Put v)

  effect Get : t
  let get () = perform Get

  let run f ~init =
    let comp =
      match f () with
      | v -> (fun s -> v)
      | effect (Put s') k -> (fun s -> continue k () s')
      | effect Get k -> (fun s -> continue k s s)
    in comp init
end

module IntState = State (struct type t = int end)

open IntState

let foo () : unit =
  printf "%d\n" (get ());
  put 42;
  printf "%d\n" (get ());
  put 21;
  printf "%d\n" (get ())

let _ = run foo 0
