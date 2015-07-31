type word = int array
type short = int array

let word_of_int n =
  let n2 = abs n in
    [|
      compare n 0;
      (n2 lsr 24) land 0x3f;
      (n2 lsr 18) land 0x3f;
      (n2 lsr 12) land 0x3f;
      (n2 lsr 06) land 0x3f;
      (n2 lsr 00) land 0x3f
    |]

let int_of_word w =
  let abs =
    ((w.(5) land 0x3f) lsl 00) +
    ((w.(4) land 0x3f) lsl 06) +
    ((w.(3) land 0x3f) lsl 12) +
    ((w.(2) land 0x3f) lsl 18) +
    ((w.(1) land 0x3f) lsl 24)
  in if w.(0) < 0 then -abs else abs

let word_of_short s =
  [|s.(0); 0; 0; 0; s.(1); s.(2)|]

let short_of_word w =
  [|w.(0); w.(4); w.(5)|]

let short_of_int n =
  short_of_word (word_of_int n)

let int_of_short s =
  int_of_word (word_of_short s)

let word_zero () = Array.make 6 0
let short_zero () = Array.make 3 0

let int_of_field w lo hi =
  let new_lo = max lo 1 in
  let rec loop value i =
    if i > hi then if lo == 0 && w.(0) < 0 then -value else value
    else loop (value + ((w.(i) land 0x3f) lsl ((hi-i)*6))) (i+1)
  in loop 0 new_lo

open Printf

let string_of_word w =
  sprintf "%s %02d %02d %02d %02d %02d"
    (if w.(0) < 0 then "-" else "+")
    w.(1) w.(2) w.(3) w.(4) w.(5)

let string_of_short s =
  sprintf "%s %02d %02d"
    (if s.(0) < 0 then "-" else "+")
    s.(1) s.(2)

let word_field w lo hi =
  let w2 = word_zero () in
  for i = max lo 1 to hi do
    w2.(i+5-hi) <- w.(i)
  done;
  if lo = 0 then w2.(0) <- w.(0);
  w2

let word_neg w =
  let w2 = Array.copy w in
    w2.(0) <- -w.(0);
    w2

let word_store dest src lo hi =
  let new_lo = max lo 1 in
  for i = new_lo to hi do
    dest.(i) <- src.(5-hi+i)
  done;
  if lo = 0 then dest.(0) <- src.(0)

let word_add w1 w2 carry =
  let n1 = int_of_word w1 in
  let n2 = int_of_word w2 in
  let n3 = n1 + n2 in
  let mag = abs n3 in
  let n4 = mag land (lnot (0x11 lsl 30)) in
  let n4 = if n3 < 0 then -n4 else n4 in
    carry := ((mag land (0x11 lsl 30)) <> 0);
    if n4 = 0 then [|w1.(0); 0; 0; 0; 0; 0|]
    else word_of_int n4
