effect E : unit

let printf = Printf.printf

let c_to_caml () =
  printf "[Caml] Enter c_to_caml\n%!";
  perform E;
  printf "[Caml] Leave c_to_caml\n%!"

let _ = Callback.register "c_to_caml" c_to_caml

external caml_to_c : unit -> unit = "caml_to_c"

let _ =
  try
    printf "[Caml] Call caml_to_c\n%!";
    caml_to_c ();
    printf "[Caml] Return from caml_to_c\n%!"
  with effect E k ->
    printf "[Caml] Handle effect E. Continuing..\n%!";
    continue k ()

