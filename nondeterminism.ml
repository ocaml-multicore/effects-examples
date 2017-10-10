(** Coin flipping -- non-determinism as an algebraic effect **)
(* This example is adapted from Kammar et. al (2013) *)

(* Non-determinism is an effect given by an operation Choose, that
   returns a boolean. *)
effect Choose : bool
let choose () = perform Choose		  

(* An example non-deterministic computation: A coin toss *)
type toss = Heads | Tails			
let toss () =
  if choose ()
  then Heads
  else Tails
  
(* Fixed interpretations *)		  
let make_charged_handler (b : bool) =
  fun m ->
  try m () with
  | effect Choose k -> continue k b
			 
let positive = make_charged_handler true  (* always interpret as true *)
let negative = make_charged_handler false (* always interpret as false *)

(* [all_results] enumerates every possible outcome of a
   non-deterministic computation *)
let all_results m =
  match m () with
  | v -> [v]
  | effect Choose k ->
     (continue k true) @ (continue (Obj.clone_continuation k) false)
(* OCaml effects/multicore only supports single-shot
   continuations. But, we can simulate multi-shot continuations by
   copying a continuation (using Obj.clone) before invocation. *)

(* Random interpretation *)
let coin m =
  try m () with
  | effect Choose k -> continue k (Random.float 1.0 > 0.5)

(* Another example: A drunken coin toss. A drunkard may fail to catch
the coin. *)
exception Too_drunk
let too_drunk () = raise Too_drunk	    
				     
let drunk_toss () =
  if choose ()
  then too_drunk ()
  else toss ()

(* This exception handler returns Some result if [m] was successful,
otherwise it returns None. *)
let optionalize m =
  try Some (m ()) with
  | Too_drunk -> None

(* This exception handler restarts [m] whenever it fails. *)		   
let rec persevere m =
  try m () with
  | Too_drunk -> persevere m

(* The pipeline operator combines two handlers [h] and [g]. Data flows
   from [g] to [h]. *)
let (-<-) h g = fun m -> h (fun () -> g m)

(* Running some examples + boilerplate conversions *)
let string_of_toss = function
  | Heads -> "Heads"
  | Tails -> "Tails"

let string_of_list string_of_e xs =
  let xs = List.map string_of_e xs in
  "[" ^ (if List.length xs > 1
	 then List.fold_left (fun xs x -> xs ^ ", " ^ x) (List.hd xs) (List.tl xs)
	 else List.hd xs)
  ^ "]"

let string_of_option string_of_e = function
  | Some e -> "Some (" ^ (string_of_e e) ^ ")"
  | None   -> "None"

let run_examples () =
  print_endline (">> positive toss   : "    ^ (string_of_toss (positive toss)));

  print_endline (">> negative toss   : "    ^ (string_of_toss (negative toss)));

  print_endline (">> all_results toss: "    ^ (string_of_list string_of_toss (all_results toss)));

  print_endline (">> coin toss       : "    ^ (string_of_toss (coin toss)));

  print_endline (">> toss |> optionalize -<- all_results       : " ^ (string_of_option (string_of_list string_of_toss) (toss |> optionalize -<- all_results)));

  print_endline (">> toss |> all_results -<- optionalize       : " ^ (string_of_list (string_of_option string_of_toss) (toss |> all_results -<- optionalize)));

  print_endline (">> drunk_toss |> optionalize -<- all_results : " ^ (string_of_option (string_of_list string_of_toss) (drunk_toss |> optionalize -<- all_results)));

  print_endline (">> drunk_toss |> all_results -<- optionalize : " ^ (string_of_list (string_of_option string_of_toss) (drunk_toss |> all_results -<- optionalize)));

  print_endline (">> drunk_toss |> optionalize -<- coin        : " ^ (string_of_option string_of_toss (drunk_toss |> optionalize -<- coin)));

  print_endline (">> drunk_toss |> peservere -<- coin          : " ^ (string_of_toss (drunk_toss |> persevere -<-  coin)));
  
