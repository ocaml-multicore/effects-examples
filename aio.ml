(* A simple echo server.
 *
 * The server listens on localhost port 9301. It accepts multiple clients and
 * echoes back to the client any data sent to the server. This server is a
 * direct-style reimplementation of the echo server found at [1], which
 * illustrates the same server written in CPS style.
 *
 * Compiling
 * ---------
 *
 * Install the multicore compiler from the OCaml Labs dev repo from [2], and do:
 *
 *   opam switch 4.02.1+multicore
 *   eval `opam config env`
 *
 * Compile the program with:
 *
 *   ocamlc -o aio unix.cma aio.ml
 *
 * Running
 * -------
 * The echo server can be tested with a telnet client by starting the server and
 * on the same machine, running:
 *
 *   telnet localhost 9301
 *
 * -----------------------
 * [1] http://www.mega-nerd.com/erikd/Blog/CodeHacking/Ocaml/ocaml_select.html
 * [2] https://github.com/ocamllabs/opam-repo-dev
 *)

open Printexc
open Printf


(* Asynchronous IO library.
 *
 * For each blocking action, if the action can be performed immediately, then it
 * is. Otherwise, the thread performing the blocking task is suspended and
 * automatically wakes up when the action completes. The suspend/resume is
 * transparent to the programmer.
 *)
module type AIO = sig

  val fork  : (unit -> unit) -> unit
  val yield : unit -> unit

  type file_descr = Unix.file_descr
  type sockaddr = Unix.sockaddr
  type msg_flag = Unix.msg_flag

  val accept : file_descr -> file_descr * sockaddr
  val recv   : file_descr -> bytes -> int -> int -> msg_flag list -> int
  val send   : file_descr -> bytes -> int -> int -> msg_flag list -> int
  val sleep  : float -> unit

  val run : (unit -> unit) -> unit
end

module Aio : AIO = struct

  type file_descr = Unix.file_descr
  type sockaddr = Unix.sockaddr
  type msg_flag = Unix.msg_flag

  (** Fork a new task. *)
  effect Fork  : (unit -> unit) -> unit
  (** Yield control. *)
  effect Yield : unit
  (** Block until data is available to read on the socket. *)
  effect Blk_read  : file_descr -> unit
  (** Block until socket is writable. *)
  effect Blk_write : file_descr -> unit
  (** Sleep for given number of seconds. *)
  effect Sleep : float -> unit

  (** Poll to see if the file descriptor is available to read. *)
  let poll_rd fd =
    let r,_,_ = Unix.select [fd] [] [] 0. in
    match r with
    | [] -> false
    | _ -> true

  (** Poll to see if the file descriptor is available to write. *)
  let poll_wr fd =
    let _,r,_ = Unix.select [] [fd] [] 0. in
    match r with
    | [] -> false
    | _ -> true

  let rec run main =
    (* Represents the queue of runnable threads *)
    let run_q = Queue.create () in

    (* Represents the threads that are waiting for read to complete *)
    let read_ht = Hashtbl.create 13 in
    (* Represents the threads that are waiting for write to complete *)
    let write_ht = Hashtbl.create 13 in
    (* Represents the threads that are sleeping. *)
    let sleep_ht = Hashtbl.create 13 in

    let enqueue k = Queue.push k run_q in

    (* Wakes up sleeping threads.
     *
     * Returns [(t,b)] where [t] is the eariest time in the future when a thread
     * needs to wake up, and [b] is true if some thread is woken up.
     *)
    let wakeup now : bool * float =
      let (l,w,n) = Hashtbl.fold (fun t k (l, w, next) ->
        if t <= now then
          (enqueue k; (t::l, true, next))
        else if t < next then
          (l, w, t)
        else (l, w, next)) sleep_ht ([], false, max_float)
      in
      List.iter (fun t -> Hashtbl.remove sleep_ht t) l;
      (w,n)
    in

    let rec dequeue () =
      if Queue.is_empty run_q then (* No runnable threads *)
        if Hashtbl.length read_ht = 0 &&
           Hashtbl.length write_ht = 0 &&
           Hashtbl.length sleep_ht = 0 then () (* We are done. *)
        else
          let now = Unix.gettimeofday () in
          let (thrd_has_woken_up, next_wakeup_time) = wakeup now in
          if thrd_has_woken_up then
            dequeue ()
          else if next_wakeup_time = max_float then
            perform_io (-1.)
          else perform_io (next_wakeup_time -. now)
      else (* Still have runnable threads *)
        continue (Queue.pop run_q) ()

    (* When there are no threads to run, perform blocking io. *)
    and perform_io timeout =
      (* Hashtbl is multivalued ==> thread-safety! *)
      let rd_fds = Hashtbl.fold (fun fd _ acc -> fd::acc) read_ht [] in
      let wr_fds = Hashtbl.fold (fun fd _ acc -> fd::acc) write_ht [] in
      let rdy_rd_fds, rdy_wr_fds, _ = Unix.select rd_fds wr_fds [] timeout in
      (* let _ = printf "%d;%d\n%!" (List.length rdy_rd_fds) (List.length * rdy_wr_fds) in *)
      let rec resume ht = function
      | [] -> ()
      | x::xs ->
          enqueue (Hashtbl.find ht x);
          Hashtbl.remove ht x;
          resume ht xs
      in
      resume read_ht rdy_rd_fds;
      resume write_ht rdy_wr_fds;
      if timeout > 0. then ignore (wakeup (Unix.gettimeofday ())) else ();
      dequeue ()
    in
    let rec core f =
      match f () with
      | () -> dequeue ()
      | exception e ->
          print_string (to_string e);
          dequeue ()
      | effect Yield k ->
          enqueue k; dequeue ()
      | effect (Fork f) k ->
          enqueue k; core f
      | effect (Blk_read fd) k ->
          if poll_rd fd then continue k ()
          else (Hashtbl.add read_ht fd k;
                dequeue ())
      | effect (Blk_write fd) k ->
          if poll_wr fd then continue k ()
          else (Hashtbl.add write_ht fd k;
                dequeue ())
      | effect (Sleep t) k ->
          if t <= 0. then continue k ()
          else (Hashtbl.add sleep_ht (Unix.gettimeofday () +. t) k;
                dequeue ())
    in
    core main

  let fork f = perform (Fork f)

  let yield () = perform Yield

  let accept fd =
    perform (Blk_read fd);
    Unix.accept fd

  let recv fd buf pos len mode =
    perform (Blk_read fd);
    Unix.recv fd buf pos len mode

  let send fd bus pos len mode =
    perform (Blk_write fd);
    Unix.send fd bus pos len mode

  let sleep t = perform (Sleep t)
end

let send sock str =
  let len = String.length str in
  let total = ref 0 in
  (try
      while !total < len do
        let write_count = Aio.send sock str !total (len - !total) [] in
        total := write_count + !total
        done
    with _ -> ()
    );
  !total

let recv sock maxlen =
  let str = Bytes.create maxlen in
  let recvlen =
    try Aio.recv sock str 0 maxlen []
    with _ -> 0
  in
  String.sub str 0 recvlen

let close sock =
  try Unix.shutdown sock Unix.SHUTDOWN_ALL
  with _ -> () ;
  Unix.close sock

let string_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (inet,port) ->
      (Unix.string_of_inet_addr inet) ^ ":" ^ (string_of_int port)

(* Repeat what the client says until the client goes away. *)
let rec echo_server sock addr =
  try
    let data = recv sock 1024 in
    if String.length data > 0 then
      (ignore (send sock data);
       echo_server sock addr)
    else
      let cn = string_of_sockaddr addr in
      (printf "echo_server : client (%s) disconnected.\n%!" cn;
       close sock)
  with
  | _ -> close sock

let server () =
  (* Server listens on localhost at 9301 *)
  let addr, port = Unix.inet_addr_loopback, 9301 in
  printf "Echo server listening on 127.0.0.1:%d\n%!" port;
  let saddr = Unix.ADDR_INET (addr, port) in
  let ssock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  (* SO_REUSEADDR so we can restart the server quickly. *)
  Unix.setsockopt ssock Unix.SO_REUSEADDR true;
  Unix.bind ssock saddr;
  Unix.listen ssock 20;
  (* Socket is non-blocking *)
  Unix.set_nonblock ssock;
  try
    (* Wait for clients, and fork off echo servers. *)
    while true do
      let client_sock, client_addr = Aio.accept ssock in
      let cn = string_of_sockaddr client_addr in
      printf "server : client (%s) connected.\n%!" cn;
      Unix.set_nonblock client_sock;
      Aio.fork (fun () -> echo_server client_sock client_addr)
    done
  with
  | _ -> close ssock

let () = Aio.run server
