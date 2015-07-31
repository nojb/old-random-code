open Str
open Globals
open Opcodes
open Words

let line_regexp = regexp "\\([a-zA-Z0-9]*\\)[ \t]+\\([a-zA-Z0-9]+\\)\\(.*\\)$"
let comment_regexp = regexp "[ \t]*\\*.*$"

let rec scan_line ic =
	let l = input_line ic in
		incr_lineno ();
		if string_match comment_regexp l 0 then begin Printf.eprintf "%d: comment found; ignoring.\n" (curr_lineno ()); scan_line ic end
		else if string_match line_regexp l 0 then
			let (x1,x2,x3) = (matched_group 1 l, matched_group 2 l, matched_group 3 l) in
				set_line x1 x2 x3;
				(x1,x2,x3)
		else begin Printf.eprintf "%d: illegal syntax; ignoring.\n" (curr_lineno ()); scan_line ic end

let eval_exp s =
	let lexbuf = Lexing.from_string s in
		Parser.exp Lexer.token lexbuf

let eval_word w =
	let lexbuf = Lexing.from_string w in
		Parser.word Lexer.token lexbuf

let eval_addr a =
	let lexbuf = Lexing.from_string a in
		Parser.addr Lexer.token lexbuf

let scan_channel ic =
	clear_loc ();
	clear_lineno ();
	clear_linecount ();
	clear_symbols ();
	try while true do
		let (x1,x2,x3) = scan_line ic in
			match x2 with
			| "EQU" -> begin add_sym x1 (eval_word x3) end
			| "ORIG" -> begin add_sym x1 (curr_loc ()); set_loc (eval_word x3) end
			| "END" -> begin signal_end (eval_word x3); raise Exit end
			| "CON" -> begin add_sym x1 (curr_loc ()); push_line (eval_word x3); incr_loc () end
			| "ALF" -> begin add_sym x1 (curr_loc ()); push_line (pack_chars (String.sub x3 1 5)); incr_loc () end
			| _ -> begin add_sym x1 (curr_loc ()); push_line (-1); incr_loc () end
	done with Exit -> () | End_of_file -> begin Printf.eprintf "Warning: no END instruction; start address set to 0.\n"; signal_end 0 end
		| _ -> Printf.eprintf "%d: unknown exception catched!" (curr_lineno ())

let compute_inst h =
	set_lineno h.lineno;
	set_loc h.address;
	let (_, x1,x2) = h.text in
		try let op = opcode_of_string x1 in h.asm <- pack_instruction op (eval_addr x2)
		with Not_found -> Printf.eprintf "%d: op `%s' is not an opcode; ignoring.\n" (curr_lineno ()) x1

let compute_program () =
	Queue.iter compute_inst program;
	push_literals ();
	Printf.eprintf "%d: END found, end_address is %d.\n" (curr_lineno ()) !end_address

let scan_program () =
	Printf.eprintf "Starting first pass...\n";
	scan_channel stdin;
	Printf.eprintf "First pass complete.\n";
	dump_symbols stderr;
	(*dump_program stdout;*)
	Printf.eprintf "Starting second pass...\n";
	compute_program ();
	Printf.eprintf "Second pass complete.\n";
	dump_program stderr
