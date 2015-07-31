open Globals
open Scanner
open Opcodes
open Words

type cpu = {
    mutable rA : int;
    mutable rX : int;
    mutable rJ : int;
    rI : int array;
    mutable overflow : bool;
    mutable cmp : int;
    mutable clock : int;
    mutable loc : int;
    mem : int array;
    mutable halted : bool;
}

let cpu = { rA = 0; rX = 0; rJ = 0; rI = Array.make 7 0; overflow = false; cmp = 0; clock = 0; loc = 0; mem = Array.make 4000 0; halted = true }

let seta n = Printf.eprintf "\t---> Setting register A = %s\n" (to_string_long n); cpu.rA <- n
let setx n = Printf.eprintf "\t---> Setting register X = %s\n" (to_string_long n); cpu.rX <- n
let seti i n = Printf.eprintf "\t---> Setting register I%d = %s\n" i (to_string_long n); cpu.rI.(i) <- n
let setj n = Printf.eprintf "\t---> Setting register J = %d\n" n; cpu.rJ <- n

let setmem idx lr n =
	let x = cpu.mem.(idx) in
		cpu.mem.(idx) <- set_field cpu.mem.(idx) lr n;
		Printf.eprintf "\t---> Setting M[%d] = %s; was %s.\n" idx (to_string_long n) (to_string_long x)

let jump n = setj cpu.loc; Printf.eprintf "\t---> Jumping to %d\n" n; cpu.loc <- n
let jump_save n = Printf.eprintf "\t---> Jumping to %d\n" n; cpu.loc <- n

let halt () = cpu.halted <- true

let setcmp x = Printf.eprintf "\t---> Setting OVERFLOW = %s\n" (if x = 0 then "[=]" else if x < 0 then "[<]" else "[>]"); cpu.cmp <- x

let comp n m =
    setcmp (compare n m)

let load_inst h =
	cpu.mem.(h.address) <- h.asm

let load_program () =
    Queue.iter load_inst program;
    cpu.loc <- !start_address;
	Printf.eprintf ">>> PROGRAM LOADED.\n"

let step () =
	let s = to_string cpu.mem.(cpu.loc) in
	let (op,a,i,fld) = unpack_instruction (cpu.mem.(cpu.loc)) in
    let aa = a + cpu.rI.(i) in
	let m = aa in
		Printf.eprintf "%04d:\t%s\t\t%-18s (M = %d I = %d F = %d)\n" cpu.loc s (disasm op a i fld) m i fld;
        cpu.loc <- cpu.loc + 1;
		match op with
		| NOP -> ()
		| LDA -> seta (get_field cpu.mem.(m) fld)
		| LDX -> setx (get_field cpu.mem.(m) fld)
		| LD1 -> seti 1 (get_field cpu.mem.(m) fld)
		| LD2 -> seti 2 (get_field cpu.mem.(m) fld)
		| LD3 -> seti 3 (get_field cpu.mem.(m) fld)
		| LD4 -> seti 4 (get_field cpu.mem.(m) fld)
		| LD5 -> seti 5 (get_field cpu.mem.(m) fld)
		| LD6 -> seti 6 (get_field cpu.mem.(m) fld)

		| STA -> setmem m fld cpu.rA
		| STX -> setmem m fld cpu.rX
		| ST1 -> setmem m fld cpu.rI.(1)
		| ST2 -> setmem m fld cpu.rI.(2)
		| ST3 -> setmem m fld cpu.rI.(3)
		| ST4 -> setmem m fld cpu.rI.(4)
		| ST5 -> setmem m fld cpu.rI.(5)
		| ST6 -> setmem m fld cpu.rI.(6)
		| STJ -> setmem m fld cpu.rJ (* sign always + ? *)
		| STZ -> setmem m 5 0

		| HLT -> halt ()

		| JMP -> jump m
		| JSJ -> jump_save m
		| JOV -> if cpu.overflow then begin cpu.overflow <- false; jump m end
		| JNOV -> if not cpu.overflow then jump m else cpu.overflow <- false
		| JL -> if cpu.cmp < 0 then jump m
		| JE -> if cpu.cmp = 0 then jump m
		| JG -> if cpu.cmp > 0 then jump m
		| JGE -> if cpu.cmp >= 0 then jump m
		| JNE -> if cpu.cmp <> 0 then jump m
		| JLE -> if cpu.cmp <= 0 then jump m
		| JAN -> if cpu.rA < 0 then jump m
		| JAZ -> if cpu.rA = 0 then jump m
		| JAP -> if cpu.rA > 0 then jump m
		| JANN -> if cpu.rA >= 0 then jump m
		| JANZ -> if cpu.rA <> 0 then jump m
		| JANP -> if cpu.rA <= 0 then jump m
		| J1N -> if cpu.rI.(1) < 0 then jump m
		| J1Z -> if cpu.rI.(1) = 0 then jump m
		| J1P -> if cpu.rI.(1) > 0 then jump m
		| J1NN -> if cpu.rI.(1) >= 0 then jump m
		| J1NZ -> if cpu.rI.(1) <> 0 then jump m
		| J1NP -> if cpu.rI.(1) <= 0 then jump m
		| J2N -> if cpu.rI.(2) < 0 then jump m
		| J2Z -> if cpu.rI.(2) = 0 then jump m
		| J2P -> if cpu.rI.(2) > 0 then jump m
		| J2NN -> if cpu.rI.(2) >= 0 then jump m
		| J2NZ -> if cpu.rI.(2) <> 0 then jump m
		| J2NP -> if cpu.rI.(2) <= 0 then jump m
		| J3N -> if cpu.rI.(3) < 0 then jump m
		| J3Z -> if cpu.rI.(3) = 0 then jump m
		| J3P -> if cpu.rI.(3) > 0 then jump m
		| J3NN -> if cpu.rI.(3) >= 0 then jump m
		| J3NZ -> if cpu.rI.(3) <> 0 then jump m
		| J3NP -> if cpu.rI.(3) <= 0 then jump m
		| J4N -> if cpu.rI.(4) < 0 then jump m
		| J4Z -> if cpu.rI.(4) = 0 then jump m
		| J4P -> if cpu.rI.(4) > 0 then jump m
		| J4NN -> if cpu.rI.(4) >= 0 then jump m
		| J4NZ -> if cpu.rI.(4) <> 0 then jump m
		| J4NP -> if cpu.rI.(4) <= 0 then jump m
		| J5N -> if cpu.rI.(5) < 0 then jump m
		| J5Z -> if cpu.rI.(5) = 0 then jump m
		| J5P -> if cpu.rI.(5) > 0 then jump m
		| J5NN -> if cpu.rI.(5) >= 0 then jump m
		| J5NZ -> if cpu.rI.(5) <> 0 then jump m
		| J5NP -> if cpu.rI.(5) <= 0 then jump m
		| J6N -> if cpu.rI.(6) < 0 then jump m
		| J6Z -> if cpu.rI.(6) = 0 then jump m
		| J6P -> if cpu.rI.(6) > 0 then jump m
		| J6NN -> if cpu.rI.(6) >= 0 then jump m
		| J6NZ -> if cpu.rI.(6) <> 0 then jump m
		| J6NP -> if cpu.rI.(6) <= 0 then jump m
		| JXN -> if cpu.rX < 0 then jump m
		| JXZ -> if cpu.rX = 0 then jump m
		| JXP -> if cpu.rX > 0 then jump m
		| JXNN -> if cpu.rX >= 0 then jump m
		| JXNZ -> if cpu.rX <> 0 then jump m
		| JXNP -> if cpu.rX <= 0 then jump m

		| INCA -> seta (cpu.rA + aa)
		| INCX -> setx (cpu.rX + aa)
		| INC1 -> seti 1 (cpu.rI.(1) + aa)
		| INC2 -> seti 2 (cpu.rI.(2) + aa)
		| INC3 -> seti 3 (cpu.rI.(3) + aa)
		| INC4 -> seti 4 (cpu.rI.(4) + aa)
		| INC5 -> seti 5 (cpu.rI.(5) + aa)
		| INC6 -> seti 6 (cpu.rI.(6) + aa)

        | DECA -> seta (cpu.rA - aa)
        | DEC1 -> seti 1 (cpu.rI.(1) - aa)
        | DEC2 -> seti 2 (cpu.rI.(2) - aa)
        | DEC3 -> seti 3 (cpu.rI.(3) - aa)
        | DEC4 -> seti 4 (cpu.rI.(4) - aa)
        | DEC5 -> seti 5 (cpu.rI.(5) - aa)
        | DEC6 -> seti 6 (cpu.rI.(6) - aa)
        | DECX -> setx (cpu.rX - aa)

		| ENTA -> seta aa (* something about the sign of m if it's zero? *)
		| ENTX -> setx aa
		| ENT1 -> seti 1 aa
		| ENT2 -> seti 2 aa
		| ENT3 -> seti 3 aa
		| ENT4 -> seti 4 aa
		| ENT5 -> seti 5 aa
		| ENT6 -> seti 6 aa
		| ENNA -> seta (-aa)
		| ENNX -> setx (-aa)
		| ENN1 -> seti 1 (-aa)
		| ENN2 -> seti 2 (-aa)
		| ENN3 -> seti 3 (-aa)
		| ENN4 -> seti 4 (-aa)
		| ENN5 -> seti 5 (-aa)
		| ENN6 -> seti 6 (-aa)

		| CMPA -> comp (get_field cpu.rA fld) (get_field cpu.mem.(m) fld)
		| CMPX -> comp (get_field cpu.rX fld) (get_field cpu.mem.(m) fld)
		| CMP1 -> comp (get_field cpu.rI.(1) fld) (get_field cpu.mem.(m) fld)
		| CMP2 -> comp (get_field cpu.rI.(2) fld) (get_field cpu.mem.(m) fld)
		| CMP3 -> comp (get_field cpu.rI.(3) fld) (get_field cpu.mem.(m) fld)
		| CMP4 -> comp (get_field cpu.rI.(4) fld) (get_field cpu.mem.(m) fld)
		| CMP5 -> comp (get_field cpu.rI.(5) fld) (get_field cpu.mem.(m) fld)
		| CMP6 -> comp (get_field cpu.rI.(6) fld) (get_field cpu.mem.(m) fld)

		| ADD -> seta (cpu.rA + (get_field cpu.mem.(m) fld))
		| SUB -> seta (cpu.rA - (get_field cpu.mem.(m) fld))
        | DIV ->
            let (a,x) = Words.div_long cpu.rA cpu.rX (get_field cpu.mem.(m) fld) in
                seta a;
                setx x

        | CHAR -> let s = Printf.sprintf "%010d" cpu.rA in
            let a = Words.pack_chars s in
            let x = Words.pack_chars (String.sub s 5 5) in
            begin
                seta (if cpu.rA < 0 then (-a) else a);
                setx (if cpu.rX < 0 then (-x) else x)
            end
        | OUT -> begin
            for i = 0 to 23 do
                for j = 1 to 5 do
                    print_char (char_of_charcode (get_field cpu.mem.(m+i) (j*8+j)))
                done
            done; print_newline ()
            end
                
		| _ -> begin Printf.eprintf "\t---> NOT IMPLEMENTED.\n"; () end

let run_program () =
    let start_time = Unix.time () in
	cpu.halted <- false;
	while not cpu.halted do
        step ()
    done;
    let end_time = Unix.time () in
        Printf.printf "elapsed time: %f\n" (end_time -. start_time)
