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
 val abort        : 'a prompt -> 'a -> 'b
end

module M : S = struct
 type ('a,'b) subcont = ('a,'b) continuation

 type 'a prompt = {
   take  : 'b. (('b, 'a) subcont -> 'a) -> 'b;
   push  : (unit -> 'a) -> 'a;
   abort : 'b. 'a -> 'b;
 }

 let new_prompt (type a) () : a prompt =
   let module M = struct effect Prompt : (('b,a) subcont -> a) -> 'b end in
   let take f  = perform (M.Prompt f)
   and push f  = match f () with  v -> v | effect (M.Prompt f) k -> f k
   and abort v = perform (M.Prompt (fun _ -> v)) in
   { take; push; abort }

 let push_prompt {push} = push
 let take_subcont {take} = take
 let push_subcont = continue
 let abort {abort} = abort
end
