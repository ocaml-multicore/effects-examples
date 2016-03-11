open Printexc

module type Applicative = sig
  type 'a t
  val pure  : 'a -> 'a t
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
end

module type Promise = sig
  include Applicative
  val fork    : (unit -> 'a) -> 'a t
  val get     : 'a t -> ('a, exn) result
  val run     : (unit -> 'a) -> ('a, exn) result
end

module Promise = struct

  type 'a cont =
    | Cont : ('a,'b) continuation -> 'a cont

  type 'a status =
    | Done of 'a
    | Cancelled of exn
    | Waiting of ('a, exn) result cont list

  type 'a t = 'a status ref

  effect Fork : (unit -> 'a) -> 'a t
  effect Wait : 'a t -> ('a, exn) result

  let fork f = perform (Fork f)

  let enqueue run_q k v =
    Queue.push (fun () -> ignore @@ continue k v) run_q

  let rec dequeue run_q =
    if Queue.is_empty run_q then ()
    else (Queue.pop run_q) ()

  let mk_status () = ref (Waiting [])

  let finish run_q sr v =
    match !sr with
    | Waiting l ->
        (List.iter (fun (Cont k) -> enqueue run_q k (Ok v)) l;
         sr := Done v)
    | _ -> failwith "Impossible: finish"

  let abort run_q sr e =
    match !sr with
    | Waiting l ->
        (List.iter (fun (Cont k) -> enqueue run_q k (Error e)) l;
         sr := Cancelled e)
    | _ -> failwith "Impossible: abort"

  let wait sr k =
    match !sr with
    | Waiting l -> sr := Waiting (Cont k::l)
    | _ -> failwith "Impossible: wait"

  let get sr =
    match !sr with
    | Done v -> Ok v
    | Cancelled e -> Error e
    | Waiting l -> perform (Wait sr)

  let run main =
    let run_q = Queue.create () in
    let rec spawn : 'a. 'a status ref -> (unit -> 'a) -> unit =
      fun sr f ->
        match f () with
        | v -> finish run_q sr v; dequeue run_q
        | exception e -> abort run_q sr e; dequeue run_q
        | effect (Wait sr) k -> wait sr k; dequeue run_q
        | effect (Fork f) k ->
            let sr = mk_status () in
            enqueue run_q k sr; spawn sr f
    in
    let sr = mk_status () in
    spawn sr main;
    get sr

end
