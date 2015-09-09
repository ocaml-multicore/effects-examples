(* One-shot multi-prompt delimited control. *)

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

  module type S = sig
    type t
    effect Eff : (('b,t) subcont -> t) -> 'b
    exception Abort of t
  end

  type 'a prompt = (module S with type t = 'a)

  let new_prompt (type a) () : a prompt =
    let module M = struct
      type t = a
      effect Eff : (('b,t) subcont -> t) -> 'b
      exception Abort of t
    end
    in (module M)

  let push_prompt (type a) (p : a prompt) f =
    let module M = (val p) in
    match f () with
    | v -> v
    | exception (M.Abort v) -> v
    | effect (M.Eff f) k -> f k

  let take_subcont (type a) (p : a prompt) f =
    let module M = (val p) in
    perform (M.Eff f)

  let push_subcont = continue

  let abort (type a) (p : a prompt) (v : a) : 'b =
    let module M = (val p) in
    raise (M.Abort v)
end
