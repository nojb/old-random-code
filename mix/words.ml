type t = int

let shift_left x n =
  let z = abs ((abs x) lsl (6*n)) in
  if x < 0 then (-z) else z

let shift_right x n =
  let z = (abs x) lsr (6*n) in
  if x < 0 then -z else z

let div_long a x y =
  let a2 = Int64.of_int (abs a) in
  let x2 = Int64.of_int (abs x) in
  let y2 = Int64.of_int (abs y) in
  let z = Int64.logor (Int64.shift_left a2 30) x2 in
  let q = Int64.div z y2 in
  let r = Int64.rem z y2 in
  let q2 = Int64.to_int q in
  let r2 = Int64.to_int r in
  ((if ((x < 0 && y > 0) || (x > 0 && y < 0)) then (-q2) else q2), (if a < 0 then (-r2) else r2))

let get_field x f =
  let l, r = f / 8, f mod 8 in
  let z =
    ((abs x) lsl (if l = 0 then 0 else (6*(l-1)+1))) lsr (if l = 0 then 30-6*r else 31-(r-l+1)*6) in
  if x < 0 && l = 0 then (-z) else z

let set_field x f y =
  let l, r = f / 8, f mod 8 in
  let x2 = abs x in
  let xl = (x2 lsr (if l = 0 then 31 else 30-(l-1)*6)) lsl (if l = 0 then 31 else 30-(l-1)*6) in
  let xr = (x2 lsl (6*r+1)) lsr (6*r+1) in
  let y2 = ((abs y) lsl (if l = 0 then (5-r)*6+1 else (4-r+l)*6+1)) lsr (if l = 0 then 1 else (l-1)*6+1) in
  let r = xl lor xr lor y2 in
  if (l = 0 && y < 0) || (l > 0 && x < 0) then (-r) else r
(*xl lor xr lor y2 lor (if l = 0 then sign y else sign x)*)

let to_string x =
  let z = abs x in
  Printf.sprintf "%c %02d %02d %02d %02d %02d"
    (if x < 0 then '-' else '+')
    ((z land 0x3F000000) lsr 24)
    ((z land 0x00FC0000) lsr 18)
    ((z land 0x0003F000) lsr 12)
    ((z land 0x00000FC0) lsr 6)
    (z land 0x0000003F)

let to_string_long = to_string

open Opcodes

let pack_instruction op (a,i,f) =
  let x = ref (opcode_number op) in
  x := set_field !x (4*8+4) (match f with None -> normal_field_setting op | Some f -> f);
  x := set_field !x (3*8+3) i;
  x := set_field !x (0*8+2) a; !x

let unpack_instruction n =
  let fld = get_field n (4*8+4) in
  let opc = get_field n (5*8+5) in
  (opcode_of_number opc fld, get_field n (0*8+2), get_field n (3*8+3), fld)

let char_table c =
  match c with
  | ' ' -> 0 | 'A' -> 1 | 'B' -> 2 | 'C' -> 3 | 'D' -> 4 | 'E' -> 5 | 'F' -> 6 | 'G' -> 7 | 'H' -> 8 | 'I' -> 9
  | 'J' -> 11 | 'K' -> 12 | 'L' -> 13 | 'M' -> 14 | 'N' -> 15 | 'O' -> 16 | 'P' -> 17 | 'Q' -> 18 | 'R' -> 19
  | 'S' -> 22 | 'T' -> 23 | 'U' -> 24 | 'V' -> 25 | 'W' -> 26 | 'X' -> 27 | 'Y' -> 28 | 'Z' -> 29 | '0' -> 30
  | '1' -> 31 | '2' -> 32 | '3' -> 33 | '4' -> 34 | '5' -> 35 | '6' -> 36 | '7' -> 37 | '8' -> 38 | '9' -> 39
  | '.' -> 40 | ',' -> 41 | '(' -> 42 | ')' -> 43 | '+' -> 44 | '-' -> 45 | '*' -> 46 | '/' -> 47 | '=' -> 48
  | '$' -> 49 | '<' -> 50 | '>' -> 51 | '@' -> 52 | ';' -> 53 | ':' -> 54 | '\'' -> 55
  | _ -> begin Printf.eprintf "Unknown code for char %c: returning 0.\n" c; 0 end

let pack_chars s =
  let rec help i x =
    if i > 5 then x
    else help (i+1) (set_field x (i*8+i) (char_table s.[i-1]))
  in help 1 0

let disasm op adr i fld =
  let l, r = fld / 8, fld mod 8 in
  Printf.sprintf "%-4s\t%d%s%s" (string_of_opcode op) adr (if i <> 0 then "," ^ (string_of_int i) else "")
    (if (normal_field_setting op) <> fld then (match field_kind op with Field -> Printf.sprintf "(%d:%d)" l r | Unit -> Printf.sprintf "(%d)" fld) else "")

let char_of_charcode c =
  match c with
  | 0 -> ' ' | 1 -> 'A' | 2 -> 'B' | 3 -> 'C' | 4 -> 'D' | 5 -> 'E' | 6 -> 'F' | 7 -> 'G'
  | 8 -> 'H' | 9 -> 'I' | 10 -> '?' | 11 -> 'J' | 12 -> 'K' | 13 -> 'L' | 14 -> 'M'
  | 15 -> 'N' | 16 -> 'O' | 17 -> 'P' | 18 -> 'Q' | 19 -> 'R' | 20 -> '?' | 21 -> '?'
  | 22 -> 'S' | 23 -> 'T' | 24 -> 'U' | 25 -> 'V' | 26 -> 'W' | 27 -> 'X'
  | 28 -> 'Y' | 29 -> 'Z' | 30 -> '0' | 31 -> '1' | 32 -> '2' | 33 -> '3' | 34 -> '4'
  | 35 -> '5' | 36 -> '6' | 37 -> '7' | 38 -> '8' | 39 -> '9' | 40 -> '.'
  | 41 -> ',' | 42 -> '(' | 43 -> ')' | 44 -> '+' | 45 -> '-' | 46 -> '*' | 47 -> '/'
  | 48 -> '=' | 49 -> '$' | 50 -> '<' | 51 -> '>' | 52 -> '@' | 53 -> ';' | 54 -> ':'
  | 55 -> '\'' | _ -> '#'
