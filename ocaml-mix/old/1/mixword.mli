type t

val to_int : t -> int
val from_int : int -> t
val get_field : t -> int -> int -> t
val get_fieldi : t -> int -> int -> int
val to_string : t -> string

val positive_zero : t

val is_zero : t -> bool

val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t * t
val div : t -> t -> t * t

val neg : t -> t
