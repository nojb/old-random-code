type word
(* type short *)

(* val word_of_short : short -> word *)
val word_of_int : int -> word
val int_of_word : word -> int
(* val short_of_int : int -> short *)
(* val word_add : word -> word -> word
val word_mul : word -> word -> word *)
val word_sign : word -> bool
val word_neg : word -> word
val word_zero : word
val word_negative_zero : word
val word_get_field : word -> int -> int -> word
(* val word_put : word -> int -> int -> word -> word *)
