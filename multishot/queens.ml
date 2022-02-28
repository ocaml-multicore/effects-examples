open Effect
open Effect.Deep

(* Ported from: https://github.com/effect-handlers/effect-handlers-bench/blob/d4a32ec337b77859c328a1103e195d3e4b3dcb5b/benchmarks/ocaml/001_nqueens/001_nqueens_ocaml.ml *)
let n = try int_of_string Sys.argv.(1) with _ -> 8

let rec safe queen diag xs =
  match xs with
  | [] -> true
  | q :: qs -> queen <> q && queen <> q + diag && queen <> q - diag &&
               safe queen (diag + 1) qs

type _ Effect.t += Pick : int -> int Effect.t
exception Fail

let rec find_solution n col : int list =
  if col = 0 then []
  else begin
    let sol = find_solution n (col - 1) in
    let queen = perform (Pick n) in
    if safe queen 1 sol then queen::sol else raise Fail
  end

let queens_count n =
  match_with (find_solution n) n {
    retc = (fun _ -> 1);
    exnc = (function Fail -> 0 | e -> raise e);
    effc = fun (type a) (e : a Effect.t) ->
      match e with
      | Pick n -> Some (fun (k : (a, _) continuation) ->
        let rec loop i acc =
          if i = n then (continue k i + acc)
          else loop (i+1) (continue (Multicont.Deep.clone_continuation k) i + acc)
        in loop 1 0)
      | _ -> None
  }

let queens_choose n =
  match_with (find_solution n) n {
    retc = (fun x -> [ x ]);
    exnc = (function Fail -> [] | e -> raise e);
    effc = fun (type a) (e : a Effect.t) ->
      match e with
      | Pick n -> Some (fun (k : (a, _) continuation) ->
        let rec loop i acc : int list list =
          if i = 1 then (continue k i @ acc)
          else loop (i-1) (continue (Multicont.Deep.clone_continuation k) i @ acc)
        in loop n [])
      | _ -> None
  }

let print_all_solutions () =
  let sols = queens_choose n in
  List.iter (fun l ->
    List.iter (fun pos -> Printf.printf "%d " pos) l;
    print_endline "") sols

let _ =
  print_all_solutions ();
  Printf.printf "%d\n" (queens_count n)