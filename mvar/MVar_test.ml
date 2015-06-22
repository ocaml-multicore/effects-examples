module MVar = MVar.Make (Sched)

open MVar
open Printf
open Sched

let mv = new_empty_mvar ()

let get_tid () = Effects.perform Get_Tid
let fork f = Effects.perform @@ Fork f

let put x =
  (printf "[Thread %d] Before put: %s\n" (get_tid ()) x;
  put_mvar x mv;
  printf "[Thread %d] After put: %s\n" (get_tid ()) x)

let get () =
  let () = printf "[Thread %d] Before get\n" (get_tid ()) in
  let v = take_mvar mv in
  let () = printf "[Thread %d] After get: %s\n" (get_tid ()) v in
  v

let main () =
  put "1";
  fork (fun () -> put "2");
  fork (fun () -> put "3");
  fork (fun () -> ignore (get ()));
  fork (fun () -> ignore (get ()));
  fork (fun () -> ignore (get ()))

let () = run main
