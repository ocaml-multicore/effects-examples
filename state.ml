open Printf

module type STATE = sig
  type t
  val put : t -> unit
  val get : unit -> t
  val run : (unit -> 'a) -> init:t -> t * 'a
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
      | x -> (fun s -> (s, x))
      | effect (Put s') k -> (fun _s -> continue k () s')
      | effect Get k -> (fun s -> continue k s s)
    in comp init
end

module IS = State (struct type t = int end)
module SS = State (struct type t = string end)

let foo () : unit =
  printf "%d\n" (IS.get ());
  IS.put 42;
  printf "%d\n" (IS.get ());
  IS.put 21;
  printf "%d\n" (IS.get ());
  SS.put "hello";
  printf "%s\n" (SS.get ());
  SS.put "world";
  printf "%s\n" (SS.get ())

let _ = IS.run (fun () -> SS.run foo ~init:"") ~init:0
