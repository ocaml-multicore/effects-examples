open Effect
open Effect.Deep

module type Applicative = sig
  type 'a t
  val pure  : 'a -> 'a t
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
end

module type Promise = sig
  include Applicative
  val fork    : (unit -> 'a) -> 'a t
  val get     : 'a t -> ('a, exn) result
  val get_val : 'a t -> 'a
  val run     : (unit -> 'a) -> ('a, exn) result
end

module Promise : Promise = struct

  type cont =
    | Cont : (unit, 'b) continuation -> cont

  type tvar = cont option ref

  let mk_tvar k = ref (Some (Cont k))

  type 'a status =
    | Done of 'a
    | Cancelled of exn
    | Waiting of tvar list

  type 'a t = 'a status ref

  type _ eff +=
   | Fork : (unit -> 'a) -> 'a t eff
   | Wait : 'a t -> unit eff

  let fork f = perform (Fork f)

  let enqueue run_q k v =
    Queue.push (fun () -> ignore @@ continue k v) run_q

  let dequeue run_q =
    if Queue.is_empty run_q then ()
    else (Queue.pop run_q) ()

  let mk_status () = ref (Waiting [])

  let finish run_q sr v =
    match !sr with
    | Waiting l ->
        sr := Done v;
        List.iter (fun tv ->
          match !tv with
          | None -> ()
          | Some (Cont k) ->
              tv := None;
              enqueue run_q k ()) l
    | _ -> failwith "Impossible: finish"

  let abort run_q sr e =
    match !sr with
    | Waiting l ->
        sr := Cancelled e;
        List.iter (fun tv ->
          match !tv with
          | None -> ()
          | Some (Cont k) ->
              tv := None;
              enqueue run_q k ()) l
    | _ -> failwith "Impossible: abort"

  let wait sr k =
    match !sr with
    | Waiting l -> sr := Waiting (mk_tvar k::l)
    | _ -> failwith "Impossible: wait"

  let rec get sr =
    match !sr with
    | Done v -> Ok v
    | Cancelled e -> Error e
    | Waiting _ -> perform (Wait sr); get sr

  let rec get_val sr =
    match !sr with
    | Done v -> v
    | Cancelled e -> raise e
    | Waiting _ -> perform (Wait sr); get_val sr

  let pure v = ref (Done v)

  let rec (<*>) f g =
    match !f, !g with
    | Cancelled _ as x, _ -> ref x
    | _, (Cancelled _ as x) -> ref x
    | Waiting _, _ ->
        begin
          perform (Wait f);
          match get f with
          | Ok f -> ref (Done f) <*> g
          | Error e -> ref (Cancelled e)
        end
    | Done f, Done g -> ref (Done (f g))
    | Done f, Waiting _ ->
        begin
          perform (Wait g);
          match get g with
          | Ok g -> ref (Done (f g))
          | Error e -> ref (Cancelled e)
        end

  let run main =
    let run_q = Queue.create () in
    let rec spawn : 'a. 'a status ref -> (unit -> 'a) -> unit =
      fun sr f ->
        match f () with
        | v -> finish run_q sr v; dequeue run_q
        | exception e -> abort run_q sr e; dequeue run_q
        | effect (Wait sr), k -> wait sr k; dequeue run_q
        | effect (Fork f), k ->
            let sr = mk_status () in
            enqueue run_q k sr; spawn sr f
    in
    let sr = mk_status () in
    spawn sr main;
    get sr

end

open Promise
open Printf

let test1 () =
  let x = pure 10 in
  let y = pure 20 in
  let z = pure (+) <*> x <*> y in
  get_val z

let _ =
  match run test1 with
  | Ok v -> Printf.printf "test1: %d\n" v
  | Error e -> Printf.printf "test2: error: %s\n" @@ Printexc.to_string e

let test2 () =
  let x = fork (fun () -> printf "test2: x\n%!"; 10) in
  let y = fork (fun () -> printf "test2: y\n%!"; raise Exit) in
  let z = fork (fun () -> printf "test2: z\n%!"; 20) in
  let add3 x y z =
    let _ = printf "test2: add %d %d %d\n" x y z in
    x + y + z
  in
  let r = pure add3 <*> x <*> y <*> z in
  get_val r

let _ =
  match run test2 with
  | Ok v -> Printf.printf "test2: %d\n" v
  | Error e -> Printf.printf "test2: error: %s\n" @@ Printexc.to_string e

let _ = print_endline "SUCCESS"
