module List = ListLabels
module String = StringLabels

open Printf

module Color = struct
  type t =
  | Blue
  | Red
  | Yellow

  let complement t t' =
  match t, t' with
    | Blue, Blue -> Blue
    | Blue, Red -> Yellow
    | Blue, Yellow -> Red
    | Red, Blue -> Yellow
    | Red, Red -> Red
    | Red, Yellow -> Blue
    | Yellow, Blue -> Red
    | Yellow, Red -> Blue
    | Yellow, Yellow -> Yellow

  let to_string = function
    | Blue -> "blue"
    | Red -> "red"
    | Yellow -> "yellow"

  let all = [ Blue; Red; Yellow ]
end

module Meeting_place = struct

  type 'chameneos t = {
    mutable state : [ `Empty | `First of 'chameneos | `Second of 'chameneos ];
    mutable meetings_left : int;
    mutex : Mutex.t;
    wait_for_second : Condition.t;
    wait_for_empty : Condition.t;
  }

  let create n = {
    state = `Empty;
    meetings_left = n;
    mutex = Mutex.create ();
    wait_for_second = Condition.create ();
    wait_for_empty = Condition.create ();
  }

  let meet t c =
    let rec loop () =
      if t.meetings_left = 0 then begin
        Condition.broadcast t.wait_for_empty;
        None
      end
      else
  match t.state with
  | `Empty ->
      t.state <- `First c;
      Condition.wait t.wait_for_second t.mutex;
      begin
        match t.state with
        | `Empty
        | `First _ ->
      assert false
        | `Second c ->
      t.state <- `Empty;
                  Condition.signal t.wait_for_empty;
                  Condition.signal t.wait_for_empty;
      Some c
      end
  | `First c1 ->
      t.state <- `Second c;
      t.meetings_left <- t.meetings_left - 1;
      Condition.signal t.wait_for_second;
      Some c1
  | `Second _ ->
      Condition.wait t.wait_for_empty t.mutex;
      loop ()
    in
    Mutex.lock t.mutex;
    let res = loop () in
    Mutex.unlock t.mutex;
    res
  ;;
end

module Chameneos = struct

  type t = {
    id : int;
    mutable color : Color.t;
    mutable meetings : int;
    mutable meetings_with_self : int;
  }

  let create =
    let id = ref 0 in
    let new_id () =
      let r = !id in
      id := r + 1;
      r
    in
    fun color ->
      { id = new_id ();
  color = color;
  meetings = 0;
  meetings_with_self = 0;
      }

  let run t place =
    let rec loop () =
      match Meeting_place.meet place t with
      | None -> ()
      | Some other ->
    t.meetings <- t.meetings + 1;
    if t.id = other.id then t.meetings_with_self <- t.meetings_with_self + 1;
    t.color <- Color.complement t.color other.color;
    loop ()
    in
    Thread.create loop ()
end

let print_complements () =
  List.iter Color.all ~f:(fun c1 ->
    List.iter Color.all ~f:(fun c2 ->
      printf "%s + %s -> %s\n"
  (Color.to_string c1)
  (Color.to_string c2)
  (Color.to_string (Color.complement c1 c2))));
  printf "\n";
;;

let spell_int i =
  let spell_char = function
    | '0' -> "zero"
    | '1' -> "one"
    | '2' -> "two"
    | '3' -> "three"
    | '4' -> "four"
    | '5' -> "five"
    | '6' -> "six"
    | '7' -> "seven"
    | '8' -> "eight"
    | '9' -> "nine"
    | x -> failwith "unexpected char"
  in
  let s = string_of_int i in
  String.iter s ~f:(fun c -> printf " %s" (spell_char c));
;;

let work colors n =
  let module C = Chameneos in
  List.iter colors ~f:(fun c -> printf " %s" (Color.to_string c)); printf "\n";
  let place = Meeting_place.create n in
  let cs = List.map colors ~f:Chameneos.create in
  let threads = List.map cs ~f:(fun c -> Chameneos.run c place) in
  List.iter threads ~f:Thread.join;
  let sum_meets = ref 0 in
  List.iter cs ~f:(fun c ->
    printf "%d" c.C.meetings; spell_int c.C.meetings_with_self; printf "\n";
    sum_meets := !sum_meets + c.C.meetings);
  spell_int !sum_meets; printf "\n";
;;


let main () =
  let n =
    try
      int_of_string (Sys.argv.(1))
    with
    | _ -> 600
  in
  print_complements ();
  let module C = Color in
  work [ C.Blue; C.Red; C.Yellow ] n;
  printf "\n";
  work [ C.Blue; C.Red; C.Yellow; C.Red; C.Yellow; C.Blue; C.Red; C.Yellow; C.Red; C.Blue ] n;
  printf "\n";
;;

let () = main ()
