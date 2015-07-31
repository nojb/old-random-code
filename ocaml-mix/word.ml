type word = int

let word_of_int n =
  if n < 0 then -n land (0x1 lsl 30)
  else n

let int_of_word n =
  if n land (0x1 lsl 30) then -(n lxor (0x1 lsl 30))
  else n

let string_of_word n =
  Printf.sprintf
    "%c %02d %02d %02d %02d %02d"
    (if n land (0x1 lsl 30) then '-' else '+')
    (word_get_field n 1 1)
    (word_get_field n 2 2)
    (word_get_field n 3 3)
    (word_get_field n 4 4)
    (word_get_field n 5 5)

let word_sign n =
  0 != (n land (0x1 lsl 30))

let word_neg n =
  n lxor (0x1 lsl 30)

let word_zero = 0

let word_negative_zero = 0x1 lsl 30

let word_get_field n i j =
  assert (i <= j)
