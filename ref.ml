open Printf

module type UNIV = sig
  type t
  val embed : unit -> ('a -> t) * (t -> 'a option)
end

module Univ : UNIV = struct
  type t = exn

  module type S = sig
    type t
    exception E of t
  end

  type 'a prop = (module S with type t = 'a)

  let create (type s) () : s prop =
    let module M = struct
      type t = s
      exception E of t
    end in
    (module M)

  let inj (type s) ((module M) : s prop) x =
    M.E x

  let proj (type s) ((module M) : s prop) y =
    match y with M.E x -> Some x | _ -> None

  let embed () = let p = create () in inj p, proj p
end

module type STATE = sig
  type 'a t

  val ref  : 'a -> 'a t
  val (!)  : 'a t -> 'a
  val (:=) : 'a t -> 'a -> unit

  val run  : (unit -> 'a) -> 'a
end

let rec find : 'a 'b. ('a -> 'b option) -> 'a list -> 'b option =
  fun f l -> match l with
      [] -> None
    | x :: xs -> match f x with None -> find f xs | s -> s

module State : STATE = struct

  type 'a t = {inj : 'a -> Univ.t; prj : Univ.t -> 'a option}

  effect Ref : 'a -> 'a t
  let ref v = perform (Ref v)

  effect Read : 'a t -> 'a
  let (!) = fun r -> perform (Read r)

  effect Write : 'a t * 'a -> unit
  let (:=) = fun r v -> perform (Write (r,v))

  let run f =
    let comp =
      match f () with
      | v -> (fun s -> v)
      | effect (Ref v) k -> (fun s ->
          let (inj, prj) = Univ.embed () in
          let cont = continue k {inj;prj} in
          cont (inj v::s))
      | effect (Read {inj; prj}) k -> (fun s ->
          match find prj s with
          | Some v -> continue k v s
          | None -> failwith "Ref.run: Impossible -> ref not found")
      | effect (Write ({inj; prj}, v)) k -> (fun s ->
          continue k () (inj v::s))
    in comp []
end

open State

let foo () =
  let r1 = ref "Hello" in
  let r2 = ref 10 in
  printf "%s\n" (!r1);
  printf "%d\n" (!r2);
  r1 := "World";
  r2 := 20;
  printf "%s\n" (!r1);
  printf "%d\n" (!r2);
  "Done"

let _ = run foo
