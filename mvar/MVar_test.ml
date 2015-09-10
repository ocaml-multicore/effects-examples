module MVar = MVar.Make (Sched)

open MVar
open Printf
open Sched

let mv = create_empty ()

let fork f = Effects.perform @@ Fork f

let put x =
  (printf "Before put: %s\n" x;
  put x mv;
  printf "After put: %s\n" x)

let get () =
  let () = printf "Before get\n" in
  let v = take mv in
  let () = printf "After get: %s\n" v in
  v

let main () =
  put "1";
  fork (fun () -> put "2");
  fork (fun () -> put "3");
  fork (fun () -> ignore (get ()));
  fork (fun () -> ignore (get ()));
  fork (fun () -> ignore (get ()))

let () = run main
