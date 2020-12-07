(** Message-passing parallel prime number generation using Sieve of Eratosthenes **)

(* A message is either a [Stop] signal or a [Candidate] prime number *)
type message = Stop | Candidate of int

let string_of_msg = function
  | Stop -> "Stop"
  | Candidate i -> Printf.sprintf "%d" i

(** Process primitives **)
type pid = int

effect Spawn : (pid -> unit) -> pid
let spawn p = perform (Spawn p)

effect Yield : unit
let yield () = perform Yield

(** Communication primitives **)

effect Send : pid * message -> unit
let send pid data =
  perform (Send (pid, data));
  yield ()

effect Recv : pid -> message option
let rec recv pid =
  match perform (Recv pid) with
  | Some m -> m
  | None -> yield (); recv pid


(** A mailbox is indexed by process ids (PIDs), each process has its own message queue **)
module Mailbox =
struct
  module Make (Ord : Map.OrderedType) =
  struct
    include Map.Make(Ord)

    let empty = empty

    let lookup key mb =
      try
        Some (find key mb)
      with
      | Not_found -> None

    let pop key mb =
      (match lookup key mb with
      | Some msg_q ->
         if Queue.is_empty msg_q then None
         else Some (Queue.pop msg_q)
       | None -> None)
      , mb

    let push key msg mb =
      match lookup key mb with
      | Some msg_q ->
         Queue.push msg msg_q;
         mb
      | None ->
         let msg_q = Queue.create () in
         Queue.push msg msg_q;
         add key msg_q mb
  end
end

(** Communication handler **)
let mailbox f =
  let module Mailbox = Mailbox.Make(struct type t = pid let compare = compare end) in
  let mailbox = ref Mailbox.empty in
  let lookup pid =
    let (msg, mb) = Mailbox.pop pid !mailbox in
    mailbox := mb; msg
  in
  match f () with
  | v -> v
  | effect (Send (pid, msg)) k ->
     mailbox := Mailbox.push pid msg !mailbox;
     continue k ()
  | effect (Recv who) k ->
     let msg = lookup who in
     continue k msg

(** Process handler
    Slightly modified version of sched.ml **)
let run main () =
  let run_q = Queue.create () in
  let enqueue k = Queue.push k run_q in
  let dequeue () =
    if Queue.is_empty run_q then ()
    else (Queue.pop run_q) ()
  in
  let pid = ref (-1) in
  let rec spawn f =
    pid := 1 + !pid;
    match f !pid with
    | () -> dequeue ()
    | effect Yield k ->
       enqueue (fun () -> continue k ()); dequeue ()
    | effect (Spawn p) k ->
       enqueue (fun () -> continue k !pid); spawn p
  in
  spawn main

let fromSome = function
  | Some x -> x
  | _ -> failwith "Attempt to unwrap None"

(** The prime number generator **)
let rec generator : pid -> unit =
  fun _ ->
    let n =
      if Array.length Sys.argv > 1
      then int_of_string Sys.argv.(1)
      else 101
    in
    let first = spawn sieve in  (* Spawn first sieve *)
    Printf.printf "Primes in [2..%d]: " n;
    for i = 2 to n do
      send first (Candidate i); (* Send candidate prime to first sieve *)
    done;
    send first Stop;            (* Stop the pipeline *)
    Printf.printf "\n"
and sieve : pid -> unit =
  fun mypid ->
    match recv mypid with
    | Candidate myprime ->
       let _ = Printf.printf "%d " myprime in
       let succ = ref None in
       let rec loop () =
         let msg = recv mypid in
         match msg with
         | Candidate prime when (prime mod myprime) <> 0 ->
            let succ_pid =
              if !succ = None then
                let pid = spawn sieve in (* Create a successor process *)
                succ := Some pid;
                pid
              else fromSome !succ
            in
            send succ_pid (Candidate prime); (* Send candidate prime to successor process *)
            loop ()
         | Stop when !succ <> None ->
            send (fromSome !succ) Stop (* Forward stop command *)
         | Stop -> ()
         | _ -> loop ()
       in
       loop ()
    | _ -> ()

(* Run application *)
let _ = mailbox (run generator)
