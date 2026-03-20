open Effect
open Effect.Deep

(* Ported from: https://github.com/effect-handlers/effect-handlers-bench/blob/d4a32ec337b77859c328a1103e195d3e4b3dcb5b/benchmarks/ocaml/001_nqueens/001_nqueens_ocaml.ml *)
let n = try int_of_string Sys.argv.(1) with _ -> 8

let rec safe queen diag xs =
  match xs with
  | [] -> true
  | q :: qs ->
      queen <> q
      && queen <> q + diag
      && queen <> q - diag
      && safe queen (diag + 1) qs

type _ eff += Pick : int -> int eff

exception Fail

let rec find_solution n col : int list =
  if col = 0 then []
  else
    let sol = find_solution n (col - 1) in
    let queen = perform (Pick n) in
    if safe queen 1 sol then queen :: sol else raise Fail

let queens_count n =
  match find_solution n n with
  | _ -> 1
  | exception Fail -> 0
  | effect (Pick n), k ->
      let rec loop i acc =
        if i = n then continue k i + acc
        else
          loop (i + 1)
            (continue (Multicont.Deep.clone_continuation k) i + acc)
      in
      loop 1 0

let queens_choose n =
  match find_solution n n with
  | x -> [ x ]
  | exception Fail -> []
  | effect (Pick n), k ->
      let rec loop i acc : int list list =
        if i = 1 then continue k i @ acc
        else
          loop (i - 1)
            (continue (Multicont.Deep.clone_continuation k) i @ acc)
      in
      loop n []

let print_all_solutions () =
  let sols = queens_choose n in
  List.iter
    (fun l ->
      List.iter (fun pos -> Printf.printf "%d " pos) l;
      print_endline "")
    sols

let _ =
  print_all_solutions ();
  Printf.printf "%d\n" (queens_count n)
