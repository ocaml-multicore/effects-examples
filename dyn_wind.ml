(* User-land dynamic wind:
   http://okmij.org/ftp/continuations/implementations.html#dynamic-wind *)

let dynamic_wind before_thunk thunk after_thunk =
  before_thunk ();
  let res =
    match thunk () with
    | v -> v
    | exception e -> after_thunk (); raise e
    | effect e k ->
        after_thunk ();
        let res' = perform e in
        before_thunk ();
        continue k res'
  in
  after_thunk ();
  res

effect E : unit

let () =
  let bt () = Printf.printf "IN\n" in
  let at () = Printf.printf "OUT\n" in
  let foo () =
    Printf.printf "peform E\n"; perform E;
    Printf.printf "peform E\n"; perform E;
    Printf.printf "done\n"
  in
  try dynamic_wind bt foo at with
  | effect E k -> Printf.printf "handled E\n"; continue k ()
