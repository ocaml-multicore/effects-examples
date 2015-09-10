type 'a t
val return  : 'a -> 'a t
val (>>)    : 'a t -> 'b t -> 'b t
val (>>=)   : 'a t -> ('a -> 'b t) -> 'b t
val yield   : unit t
val fork    : unit t -> unit t
val run     : unit t -> unit
val atom    : (unit -> unit) -> unit t

type 'a cont
type ready_cont
val prepare : 'a cont -> 'a -> ready_cont
val suspend : ('a cont -> (('a * ready_cont option) option)) -> 'a t

val iter_p : ('a -> unit t) -> 'a list -> unit t
val map_p : ('a -> 'b t) -> 'a list -> 'b list t
