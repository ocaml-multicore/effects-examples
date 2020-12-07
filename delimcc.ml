(* One-shot multi-prompt delimited control :
   http://okmij.org/ftp/continuations/implementations.html *)

module type S = sig
 type 'a prompt
 (* One-shot continuation. *)
 type ('a,'b) subcont

 val new_prompt   : unit -> 'a prompt
 val push_prompt  : 'a prompt -> (unit -> 'a) -> 'a
 val take_subcont : 'b prompt -> (('a,'b) subcont -> 'b) -> 'a
 val push_subcont : ('a,'b) subcont -> 'a -> 'b

 (* Assorted control operators *)
 val reset    : ('a prompt -> 'a) -> 'a
 val shift    : 'a prompt -> (('b -> 'a) -> 'a) -> 'b
 val control  : 'a prompt -> (('b -> 'a) -> 'a) -> 'b
 val shift0   : 'a prompt -> (('b -> 'a) -> 'a) -> 'b
 val control0 : 'a prompt -> (('b -> 'a) -> 'a) -> 'b
 val abort    : 'a prompt -> 'a -> 'b
end

module M : S = struct
 type ('a,'b) subcont = ('a,'b) continuation

 type 'a prompt = {
   take  : 'b. (('b, 'a) subcont -> 'a) -> 'b;
   push  : (unit -> 'a) -> 'a;
 }

 let new_prompt (type a) () : a prompt =
   let module M = struct effect Prompt : (('b,a) subcont -> a) -> 'b end in
   let take f  = perform (M.Prompt f) in
   let push f  = match f () with  v -> v | effect (M.Prompt f) k -> f k in
   { take; push }

 let push_prompt prompt = prompt.push
 let take_subcont prompt = prompt.take
 let push_subcont k v =
   let k' = Obj.clone_continuation k in
   continue k' v

 (** For the details of the implementation of control and shift0, see
     https://hackage.haskell.org/package/CC-delcont-0.2.1.0/docs/src/Control-Monad-CC.html *)
 let reset e = let p = new_prompt () in push_prompt p (fun () -> e p)
 let shift p f = take_subcont p (fun sk -> push_prompt p (fun () -> f (fun c -> push_prompt p (fun () -> push_subcont sk c))))
 let control p f = take_subcont p (fun sk -> push_prompt p (fun () -> f (fun c -> push_subcont sk c)))
 let shift0 p f = take_subcont p (fun sk -> f (fun c -> push_prompt p (fun () -> push_subcont sk c)))
 let control0 p f = take_subcont p (fun sk -> f (fun c -> push_subcont sk c))
 let abort p e = take_subcont p (fun _ -> e)
end

open M;;

let p = new_prompt ();;

assert ([] = push_prompt p (fun () -> 1::2::take_subcont p (fun _ -> [])));;
assert ([1;2] = push_prompt p (fun () -> 1::2::take_subcont p (fun k -> push_subcont k [])));;
assert (135 =
  let p1 = new_prompt () in
  let p2 = new_prompt () in
  let p3 = new_prompt () in
  let pushtwice sk =
    sk (fun () ->
      sk (fun () ->
        shift0 p2 (fun sk2 -> sk2 (fun () ->
                    sk2 (fun () -> 3))) ()))
  in
  push_prompt p1 (fun () ->
    push_prompt p2 (fun () ->
      push_prompt p3 (fun () -> shift0 p1 pushtwice ()) + 10) + 1) + 100)
