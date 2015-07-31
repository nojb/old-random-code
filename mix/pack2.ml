let field_mask l r =
	match l, r with
	| 0,0 -> 0x00000000 | 0,1 -> 0x3F000000 | 0,2 -> 0x3FFC0000 | 0,3 -> 0x3FFFF000 | 0,4 -> 0x3FFFFFC0 | 0,5 -> 0x3FFFFFFF
	| 1,1 -> 0x3F000000 | 1,2 -> 0x3FFC0000 | 1,3 -> 0x3FFFF000 | 1,4 -> 0x3FFFFFC0 | 1,5 -> 0x3FFFFFFF
	| 2,2 -> 0x00FC0000 | 2,3 -> 0x00FFF000 | 2,4 -> 0x00FFFFC0 | 2,5 -> 0x00FFFFFF
	| 3,3 -> 0x0003F000 | 3,4 -> 0x0003FFC0 | 3,5 -> 0x0003FFFF
	| 4,4 -> 0x00000FC0 | 4,5 -> 0x00000FFF
	| 5,5 -> 0x0000003F | _ -> failwith "unknown mask"

let get_field n (l,r) =
	assert (l <= r);
	let x = abs n in
	let x2 = (x land field_mask l r) lsr ((5-r)*6) in
		if (n < 0) && (l = 0) then -x2 else x2

let set_field n (l,r) z =
	assert (l <= r);
	let x = abs n in
	let w = abs z in
	let x2 = x land (lnot (field_mask l r)) in
	let x3 = x2 lor ((w land field_mask (5-(r-l)) 5) lsl ((5-r)*6)) in
		if (l > 0 && n < 0) || (l = 0 && z < 0) then -x3 else x3

let string_of_word n =
	Printf.sprintf "%c %2d %2d %2d %2d %2d (%d)" (if n < 0 then '-' else '+') (get_field n (1,1)) (get_field n (2,2)) (get_field n (3,3)) (get_field n (4,4)) (get_field n (5,5)) n

let print_word n = print_endline (string_of_word n)

let string_of_inst n =
	Printf.sprintf "%c %04d %02d %02d %02d" (if n < 0 then '-' else '+') (get_field n (1,2)) (get_field n (3,3)) (get_field n (4,4)) (get_field n (5,5))

let print_inst n = print_endline (string_of_inst n)

let unpack_field n = (n / 8, n mod 8)

open Opcodes

let pack_instruction op (a,i,f) =
	let x = ref (opcode_number op) in
		x := set_field !x (4,4) (match f with None -> normal_field_setting op | Some f -> f);
		x := set_field !x (3,3) i;
		x := set_field !x (0,2) a; !x

let unpack_instruction n =
	let fld = get_field n (4,4) in
	let opc = get_field n (5,5) in
		(opcode_of_number opc fld, get_field n (0,2), get_field n (3,3), fld)

let char_table c =
	match c with
	| ' ' -> 0 | 'A' -> 1 | 'B' -> 2 | 'C' -> 3 | 'D' -> 4 | 'E' -> 5 | 'F' -> 6 | 'G' -> 7 | 'H' -> 8 | 'I' -> 9
	| 'J' -> 11 | 'K' -> 12 | 'L' -> 13 | 'M' -> 14 | 'N' -> 15 | 'O' -> 16 | 'P' -> 17 | 'Q' -> 18 | 'R' -> 19
	| 'S' -> 22 | 'T' -> 23 | 'U' -> 24 | 'V' -> 25 | 'W' -> 26 | 'X' -> 27 | 'Y' -> 28 | 'Z' -> 29 | '0' -> 30
	| '1' -> 31 | '2' -> 32 | '3' -> 33 | '4' -> 34 | '5' -> 35 | '6' -> 36 | '7' -> 37 | '8' -> 38 | '9' -> 39
	| '.' -> 40 | ',' -> 41 | '(' -> 42 | ')' -> 43 | '+' -> 44 | '-' -> 45 | '*' -> 46 | '/' -> 47 | '=' -> 48
	| '$' -> 49 | '<' -> 50 | '>' -> 51 | '@' -> 52 | ';' -> 53 | ':' -> 54 | '\'' -> 55 | _ -> begin Printf.eprintf "Unknown code for char %c: returning 0.\n" c; 0 end

let pack_chars s =
	let rec help i x =
		if i > 5 then x
		else help (i+1) (set_field x (i,i) (char_table s.[i]))
	in help 1 0

let disasm op adr i fld =
	let (l,r) = unpack_field fld in
		Printf.sprintf "%-4s\t%d%s%s" (string_of_opcode op) adr (if i <> 0 then "," ^ (string_of_int i) else "")
			(if (normal_field_setting op) <> fld then (match field_kind op with Field -> Printf.sprintf "(%d,%d)" l r | Unit -> Printf.sprintf "(%d)" fld) else "")
