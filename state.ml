open Printf
(* From: https://gist.github.com/kayceesrk/3c307d0340fbfc68435d4769ad447e10 *)
open Effect
open Effect.Deep

module type STATE = sig
  type t
  val put : t -> unit
  val get : unit -> t
  val run : (unit -> 'a) -> init:t -> t * 'a
end

module State (S : sig type t end) : STATE with type t = S.t = struct

  type t = S.t

  type _ eff += Put : t -> unit eff
  let put v = perform (Put v)

  type _ eff += Get : t eff
  let get () = perform Get

  let run (type a) (f : unit -> a) ~init : t * a =
    let comp =
      match_with f ()
      { retc = (fun x -> (fun s -> (s, x)));
        exnc = (fun e -> raise e);
        effc = fun (type b) (e : b eff) ->
                 match e with
                 | Get -> Some (fun (k : (b, t -> (t * a)) continuation) ->
                     (fun (s : t) -> continue k s s))
                 | Put s' -> Some (fun k ->
                     (fun _s -> continue k () s'))
                 | e -> None
       }
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