type 'a t

val map : ('a -> 'b) -> 'a t -> 'b t
val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val is_empty : 'a t -> bool
val append : 'a t -> 'a t -> 'a t
val cons : 'a -> 'a t Lazy.t -> 'a t
val hd : 'a t -> 'a
val tl : 'a t -> 'a t
val filter : ('a -> bool) -> 'a t -> 'a t
val nth : int -> 'a t -> 'a
val interleave : 'a t -> 'a t -> 'a t
val flatten : 'a t t -> 'a t
val flatmap : ('a -> 'b) -> 'a t t -> 'b t
val empty : 'a t
val singleton : 'a -> 'a t
