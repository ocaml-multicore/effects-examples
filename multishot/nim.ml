(* Nim game (https://en.wikipedia.org/wiki/Nim)
   It was Nicolas Oury's original idea to use Nim to show case handlers.
   c.f. https://github.com/slindley/effect-handlers/blob/master/Examples/Nim.hs

   This particular implementation is adapted from Hillerström and Lindley.

 Mathematical game Nim

 Rules:
  - Two players: Alice and Bob; Alice always starts.
  - One heap of N sticks.
  - Turn-based, one move per turn.
  - A player may pick between 1-3 sticks at each turn.
  - The player, who takes the last stick, wins.
 *)
open Effect
open Effect.Deep

(* Data type modelling the players *)
type player = Alice | Bob

(* String representation of players *)
let string_of_player = function
  | Alice -> "Alice"
  | Bob   -> "Bob"

(* The [move] operation is centric to the game. The operation is
parameterised by the active player and the number of sticks left in
the game. *)
type _ eff += Move : (player * int) -> int eff
let move p n = perform (Move (p, n))

(* The game is modelled as two mutually recursive functions *)
let rec alice_turn n =
  if n == 0
  then Bob
  else bob_turn (n - (move Alice n))
and bob_turn n =
  if n == 0
  then Alice
  else alice_turn (n - (move Bob n))

(* Auxiliary function to start a game with [n] sticks. *)
let game n =
  fun () -> alice_turn n

(** Encoding player strategies **)
(* The strategy handler assigns strategy s(p) to player [p] *)
let strategy (s : player -> (int -> (int, player) continuation -> player)) m =
  try_with
    m ()
  { effc = fun (type a) (e : a eff) -> 
      match e with
      | Move (p, n) -> Some (fun (k : (a, player) continuation) -> s p n k)
      | _ -> None }

(* Simple (and naive) strategy *)
let ns _ k = continue k 1

(* The perfect strategy *)
let ps n k = continue k (max 1 (n mod 4))

(* Brute force strategy *)
(* The auxiliary function [valid_moves] computes the set of legal
moves when there are [n] sticks left in the game. *)
let valid_moves n =
  List.filter (fun m -> m <= n) [1;2;3]

(* The function [elem_index] returns Some index of the first element
satisfying the predicate [p]. *)
let elem_index p xs =
  let rec elem_index' i = function
    | x :: xs when p x -> Some i
    | x :: xs -> elem_index' (i+1) xs
    | []      -> None
  in
  elem_index' 0 xs

(* Nonlinear continue invokes a copy of [k] *)
let nonlinear_continue k = continue (Multicont.Deep.clone_continuation k)

(* This function maps a continuation [k] over a list *)
let rec mapk k = function
  | x :: xs -> (nonlinear_continue k x) :: mapk k xs
  | []      -> []

(* Finally, we can define the brute force strategy. In contrast to
[ns] and [ps] it takes an additional parameter [p] which is the player
for whom we are attempting to brute force a winning strategy. *)
let bf p n k =
  let winners = mapk k (valid_moves n) in
  match elem_index (fun w -> w == p) winners with
  | None   -> continue k 1     (* Not among the winners *)
  | Some i -> continue k (i+1) (* Among the winners, play the winning strategy (indices are zero-based) *)

(* Some example strategy handlers *)
let naive   = strategy (fun _ -> ns)
let perfect = strategy (fun _ -> ps)
let bruteforce_bob = strategy (function | Alice -> ps
					| Bob   -> bf Bob)

(** Computing game data **)
(* The strategy handlers produce a single piece of data about games,
namely, the winner of a particular game. We can generalise this idea
to compute the game tree of a game. *)

type gametree = Winner of player
	      | Take   of player * (int * gametree) list

(* String representation of a gametree *)
let rec string_of_gametree : gametree -> string =
  function
  | Winner p     -> "Winner(" ^ (string_of_player p) ^ ")"
  | Take (p, ts) -> "Take" ^ (string_of_pair string_of_player (string_of_list (string_of_pair string_of_int string_of_gametree)) (p, ts))
and string_of_pair : 'a 'b. ('a -> string) -> ('b -> string) -> ('a * 'b) -> string =
  fun string_of_x string_of_y (x,y) -> "(" ^ (string_of_x x) ^ ", " ^ (string_of_y y) ^ ")"
and string_of_list string_of_x xs = "[" ^ (String.concat "; " (List.map string_of_x xs)) ^ "]"


(* A zip that zips until either list has been exhausted. *)
let rec zip xs ys =
  match xs, ys with
  | [], _ -> []
  | _, [] -> []
  | (x :: xs), (y :: ys) -> (x, y) :: (zip xs ys)

(* This function reifies a move as a node in the game tree *)
let reify p n k =
  let subgames = mapk k (valid_moves n) in
  let subtrees = zip [1;2;3] subgames in
  Take (p, subtrees)

let gametree m =
  match_with m () {
    retc = (fun v -> Winner v);
    exnc = (fun e -> raise e);
    effc = fun (type a) (e : a eff) ->
      match e with 
      | Move (p, n) -> Some (fun (k : (a, _) continuation) -> reify p n k)
      | _ -> None
  }

(** Cheat detection via effect forwarding **)
(* We model Cheat as an exception parameterised by the player (the
cheater) and the number of sticks the player took *)
exception Cheat of player * int
let cheat p n = raise (Cheat (p, n))

(* A simple cheating strategy is to take all sticks, thereby winning
   in a single move *)
let cs n k = continue k n

let bob_cheats = strategy (function | Alice -> ps
	   			    | Bob -> cs)

(* The cheat detection mechanism *)
let check_move p n k =
  let m = move p n in
  if m < 1 || 3 < m
  then cheat p m    (* player p cheats by making an illegal move m (m < 1 or 3 < m) *)
  else continue k m

let checker m =
  try_with m () {
    effc = fun (type a) (e : a eff) ->
      match e with
      | Move (p, n) -> Some (fun (k : (a, _) continuation) -> check_move p n k)
      | _ -> None
  }

(* The following exception handler reports cheaters *)
let cheat_report m =
  try m () with
  | Cheat (p, n) -> failwith ("Cheater: " ^ (string_of_player p) ^ " took " ^ (string_of_int n) ^ " sticks!")

(* Another way to deal with cheaters is to disqualify them *)
let cheat_lose m =
  try m () with
  | Cheat (Alice, _) -> Bob
  | Cheat (Bob, _)   -> Alice

(* The pipeline operator combines two handlers [h] and [g]. Data flows
   from [g] to [h]. *)
let (-<-) h g = fun m -> h (fun () -> g m)

(** Choosing between strategies **)
type _ eff += Choose : bool eff
let choose () = perform Choose

(* Flip a coin to decide whether to interpret Choose as true or
false *)
let coin m =
  try_with m () {
    effc = fun (type a) (e : a eff) ->
      match e with
      | Choose -> Some (fun (k : (a, _) continuation) -> continue k (Random.float 1.0 > 0.5))
      | _ -> None
  }

let bob_maybe_cheats m =
  let h = if choose ()
	  then strategy (fun _ -> ps)
	  else strategy (function
			 | Alice -> ps
			 | Bob   -> cs)
  in h m

(** Stateful scoreboard **)
(* The state effect is given by two operations
    1) get to retrieve the current state,
    2) and put to update the state *)
(* State module is copied from KC's state example *)
module type STATE = sig
  type t
  val put : t -> unit
  val get : unit -> t
  val run : (unit -> 'a) -> init:t -> 'a
end

(* From: https://gist.github.com/kayceesrk/3c307d0340fbfc68435d4769ad447e10 *)
module State (S : sig type t end) : STATE with type t = S.t = struct
  type t = S.t

  type _ eff += Put : t -> unit eff
  let put v = perform (Put v)

  type _ eff += Get : t eff
  let get () = perform Get

  let run (type a) (f : unit -> a) ~init : a =
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
    in snd @@ comp init
end

type gamestate = (player * int) list
module GS = State (struct type t = gamestate end)

(* Get and put operations *)
let get = GS.get
let put = GS.put

(* State handler with seed [s] *)
let state s m = GS.run m ~init:s

(* Initially both players have zero wins *)
let s0 = [(Alice,0); (Bob,0)]

(* Update scoreboard *)
let increment_wins p = List.map (fun (p',n) -> if p == p' then (p',n+1) else (p',n))

(* Post-processing handler that updates the scoreboard *)
let score_updater m =
  match m () with
  | p -> put (increment_wins p (get ()))

(* Print the scoreboard *)
let print_board s =
  let rec make_whitespace n =
    if n > 0
    then " " ^ (make_whitespace (n - 1))
    else ""
  in
  let s = List.map
	    (fun (p,n) ->
	      let player  = string_of_player p in
	      let wins    = string_of_int n in
	      "| " ^ player ^ make_whitespace (11 - (String.length player)) ^
		"|" ^ make_whitespace (8 - (String.length wins)) ^ wins ^ " |"
	    ) (List.sort (fun x y -> let (n,n') = (snd x, snd y) in
				     if n < n' then 1
				     else if n > n' then -1 else 0 )
			 s)
  in
  print_endline("/======================\\");
  print_endline("|     NIM HIGHSCORE    |");
  print_endline("|======================|");
  print_endline("|   Player   |  #Wins  |");
  print_endline("|============|=========|");
  (if List.length s > 1
   then (print_endline (List.hd s);
	 List.fold_left
	   (fun _ l ->
	     print_endline("|============|=========|");
	     print_endline l;
	   ) () (List.tl s))
   else ());
  print_endline("\\======================/")

(* Post-processing handler that prints the scoreboard *)
let printer m =
  match m () with
  | _ -> print_board (get ())

(* Replays a game after n times *)
let rec replay n m =
  match m () with
  | _ when n > 0 -> replay (n-1) m
  | x -> x

let run_examples () =
  print_endline (">> game 7 |> perfect  :\n" ^ (string_of_player (game 7  |> perfect)));
  print_endline (">> game 12 |> perfect :\n" ^ (string_of_player (game 12 |> perfect)));

  (* Computing game tree *)
  print_endline (">> game 3 |> gametree:\n" ^ (string_of_gametree (game 3 |> gametree)));

  (* A stateful scoreboard *)
  print_endline ">> game 7 |> (state s0) -<- printer -<- (replay 10) -<- coin -<- score_updater -<- bob_maybe_cheats :";
  let _ = game 7 |> (state s0) -<- printer -<- (replay 10) -<- coin -<- score_updater -<- bob_maybe_cheats in

  (* Cheat detection example *)
  print_endline ">> game 7 |> cheat_report -<- bob_cheats -<- checker :\n";
  let _ = game 7 |> cheat_report -<- bob_cheats -<- checker in
  ()

let _ = run_examples ()
