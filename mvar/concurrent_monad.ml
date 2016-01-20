(* Demonstrate the concurrent scheduler *)

let log = Printf.printf

(*   ************
      Fiber tree
     ************
           0
         /  \
        1    2
       / \  / \
      3   4 5  6
*)

open Sched_monad

let (>>) a b = a >>= (fun _ -> b)

let rec f id depth =
  log "Starting number %i\n%!" id;
  if depth > 0 then begin
    log "Forking (1) number %i\n%!" (id * 2 + 1);
    fork (f (id * 2 + 1) (depth - 1)) >>
    let () = log "Forking (2) number %i\n%!" (id * 2 + 2) in
    fork (f (id * 2 + 2) (depth - 1))
  end
  else begin
    log "Yielding in number %i\n%!" id;
    yield >>= fun _ ->
    log "Resumed number %i\n%!" id;
    return ()
  end;
  log "Finishing number %i\n%!" id;
  return ()

let () = run (f 0 2)
