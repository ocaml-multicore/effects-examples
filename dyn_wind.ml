let dynamic_wind before_thunk thunk after_thunk =
  before_thunk ();
  let res =
    match thunk () with
    | v -> v
    | exception e -> after_thunk (); raise e
    | effect e k ->
        let res' = perform e in
        before_thunk ();
        let res'' = continue k res' in
        after_thunk ();
        res''
  in
  after_thunk ();
  res

type 'a ilist =
  | Nil
  | Val of 'a * 'a ilist
  | List of 'a ilist * 'a ilist

let indent_level = ref 0

let rec shift = function
  | 0 -> ()
  | n -> print_string "\t"; shift (n-1)

let rec display show = function
  | Nil -> ()
  | Val (v,l) ->
      let str = show v in
      shift !indent_level;
      print_string str;
      print_string "\n";
      display show l
  | List (l,l') ->
      incr indent_level;
      display show l;
      decr indent_level;
      display show l'

let l1 = Val(0, Val(1,
           List ( Val(-2, Val(3, Nil)),
         Val(4, Nil))))

let () = Printf.printf "--------Display 1--------\n"
let () = display string_of_int l1
