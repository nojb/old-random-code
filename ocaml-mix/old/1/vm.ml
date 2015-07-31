type label_t =
  | Here of int
  | Lit of string

type op_t =
  | ALF
  | CON
  | END
  | OPCODE of int

type line_t = {
  label : label_t;
  op : op_t;
  address : string;
  text : string;
  line_number : int;
  mutable loc : int option;
  mutable asm : MixWord.t;
}

val assemble : in_channel -> line_t array

let assembler_state = {
  mutable pc : int;
  backpatching : (label_t, line_t) Hashtbl.t;
  here_labels : (int, line_t) Hashtbl.t;
  labels : (string, line_t) Hashtbl.t;
}

let assemble_line state line =
  match line.op with
  | OPCODE -> ...
  | ORIG -> begin
    define_label state line.label;
    state.loc <- MixWord.to_int (eval_wval line.address)
  end
  | END -> begin
    state.orig <- MixWord.getfieldi (eval_wval line.address) 4 5
    raise End
  end
  | CON -> begin
    state.loc <- state.loc + 1;
  end
  | EQU -> begin
    define_label line.label (eval_wvallll
  end

let assemble ic =
  let read_lines () =
    let rec loop x =
      try loop ((input_line ic)::x)
      with End_of_file -> Array.of_list (List.rev x)
    in loop []
  in let lines = Array.map split_line (read_lines ()) in
  Array.iter (assemble_line state) lines; lines

type vm = {
  mem : Mixword.t array;
  rI : Mixword.t array;
  mutable rA : Mixword.t;
  mutable rX : Mixword.t;
  mutable rJ : Mixword.t;
  mutable cmp : int;
  mutable over : bool;
  mutable pc : int;
  mutable halted : bool;
};;

let make =
  { mem = Array.make 4000 Mixword.positive_zero;
    rI = Array.make 7 Mixword.positive_zero;
    rA = Mixword.positive_zero;
    rX = Mixword.positive_zero;
    rJ = Mixword.positive_zero;
    cmp = 0;
    over = false;
    pc = 0;
    halted = true; }
;;

let fetch vm =
  let x = vm.mem[vm.pc] in
    vm.pc <- vm.pc + 1
    x
;;

let run vm =
  vm.halted <- false;
  while not vm.halted do
    step vm
  done
;;

let nop vm =
  ()

let add vm =
  ()

let sub vm =
  ()

let mul vm =
  ()

let mul vm =
  ()

let div vm =
  ()

let num vm =
  ()

let char_ vm =
  ()

let hlt vm =
  vm.time <- vm.time + 10;
  vm.halted <- true
;;

let sla vm =
  ()

let sra vm =
  ()

let slax vm =
  ()

let srax vm =
  ()

let slc vm =
  ()

let src vm =
  ()

let move vm =
  ()

let lda vm =
  ()

let ld1 vm =
  ()

let ld2 vm =
  ()

let ld3 vm =
  ()

let ld4 vm =
  ()

let ld5 vm =
  ()

let ld6 vm =
  ()

let ldx vm =
  ()

let ldan vm =
  ()

let ld1n vm =
  ()

let ld2n vm =
  ()

let ld3n vm =
  ()

let ld4n vm =
  ()

let ld5n vm =
  ()

let ld6n vm =
  ()

let ldxn vm =
  ()

let sta vm =
  ()

let st1 vm =
  ()

let st2 vm =
  ()

let st3 vm =
  ()

let st4 vm =
  ()

let st5 vm =
  ()

let st6 vm =
  ()

let stx vm =
  ()

let stz vm =
  ()

let stj vm =
  ()

let jbus vm =
  ()

let ioc vm =
  ()

let in_ vm =
  ()

let out vm =
  ()

let jred vm =
  ()

let jmp vm =
  ()

let jsj vm =
  ()

let dispatch vm op =
  let c = Mixword.get_fieldi op 5 5 in
  let f = Mixword.get_fieldi op 4 4 in
  let i = Mixword.get_fieldi op 3 3 in
  let a = Mixword.get_fieldi op 0 2 in
  let m = a + Mixword.to_int vm.rI[i] in
    match c with
    | 0 -> nop vm
    | 1 -> add vm
    | 2 -> sub vm
    | 3 -> mul vm
    | 4 -> div vm
    | 5 -> (match f with
        | 0 -> num vm
        | 1 -> char_ vm
        | 2 -> hlt vm)
    | 6 -> (match f with
        | 0 -> sla vm
        | 1 -> sra vm
        | 2 -> slax vm
        | 3 -> srax vm
        | 4 -> slc vm
        | 5 -> src vm)
    | 7 -> (match f with
        | 1 -> move vm)
    | 8 -> lda vm
    | 9 -> ld1 vm
    | 10 -> ld2 vm
    | 11 -> ld3 vm
    | 12 -> ld4 vm
    | 13 -> ld5 vm
    | 14 -> ld6 vm
    | 15 -> ldx vm
    | 16 -> ldan vm
    | 17 -> ld1n vm
    | 18 -> ld2n vm
    | 19 -> ld3n vm
    | 20 -> ld4n vm
    | 21 -> ld5n vm
    | 22 -> ld6n vm
    | 23 -> ldxn vm
    | 24 -> sta vm
    | 25 -> st1 vm
    | 26 -> st2 vm
    | 27 -> st3 vm
    | 28 -> st4 vm
    | 29 -> st5 vm
    | 30 -> st6 vm
    | 31 -> stx vm
    | 33 -> stz vm
    | 32 -> (match f with
        | 2 -> stj vm)
    | 34 -> (match f with
        | 0 -> jbus vm)
    | 35 -> (match f with
        | 0 -> ioc vm)
    | 36 -> (match f with
        | 0 -> in_ vm)
    | 37 -> (match f with
        | 0 -> out vm)
    | 38 -> (match f with
        | 0 -> jred vm)
    | 39 -> (match f with
        | 0 -> jmp vm
        | 1 -> jsj vm
        | 2 -> jov vm
        | 3 -> jnov vm
        | 4 -> jl vm
        | 5 -> je vm
        | 6 -> jg vm
        | 7 -> jge vm
        | 8 -> jne vm)
    | 40 -> (match f with
        | 0 -> jan vm
        | 1 -> jaz vm
        | 2 -> jap vm
        | 3 -> jann vm
        | 4 -> janz vm
        | 5 -> janp vm)
    | 41 -> (match f with
        | 0 -> j1n vm
        | 1 -> j1z vm
        | 2 -> j1p vm
        | 3 -> j1nn vm
        | 4 -> j1nz vm
        | 5 -> j1np vm)
    | 42 -> (match f with
        | 0 -> j2n vm
        | 1 -> j2z vm
        | 2 -> j2p vm
        | 3 -> j2nn vm
        | 4 -> j2nz vm
        | 5 -> j2np vm)
    | 43 -> (match f with
        | 0 -> j3n vm
        | 1 -> j3z vm
        | 2 -> j3p vm
        | 3 -> j3nn vm
        | 4 -> j3nz vm
        | 5 -> j3np vm)
    | 44 -> (match f with
        | 0 -> j4n vm
        | 1 -> j4z vm
        | 2 -> j4p vm
        | 3 -> j4nn vm
        | 4 -> j4nz vm
        | 5 -> j4np vm)
    | 45 -> (match f with
        | 0 -> j5n vm
        | 1 -> j5z vm
        | 2 -> j5p vm
        | 3 -> j5nn vm
        | 4 -> j5nz vm
        | 5 -> j5np vm)
    | 46 -> (match f with
        | 0 -> j6n vm
        | 1 -> j6z vm
        | 2 -> j6p vm
        | 3 -> j6nn vm
        | 4 -> j6nz vm
        | 5 -> j6np vm)
    | 47 -> (match f with
        | 0 -> jxn vm
        | 1 -> jxz vm
        | 2 -> jxp vm
        | 3 -> jxnn vm
        | 4 -> jxnz vm
        | 5 -> jxnp vm)
    | 48 -> (match f with
        | 0 -> inca vm)
        | 1 -> deca vm
        | 2 -> enta vm
        | 3 -> enna vm
    | 49 -> (match f with
        | 0 -> inc1 vm)
        | 1 -> dec1 vm
        | 2 -> ent1 vm
        | 3 -> enn1 vm
    | 50 -> (match f with
        | 0 -> inc2 vm)
        | 1 -> dec2 vm
        | 2 -> ent2 vm
        | 3 -> enn2 vm
    | 51 -> (match f with
        | 0 -> inc3 vm)
        | 1 -> dec3 vm
        | 2 -> ent3 vm
        | 3 -> enn3 vm
    | 52 -> (match f with
        | 0 -> inc4 vm)
        | 1 -> dec4 vm
        | 2 -> ent4 vm
        | 3 -> enn4 vm
    | 53 -> (match f with
        | 0 -> inc5 vm)
        | 1 -> dec5 vm
        | 2 -> ent5 vm
        | 3 -> enn5 vm
    | 54 -> (match f with
        | 0 -> inc6 vm)
        | 1 -> dec6 vm
        | 2 -> ent6 vm
        | 3 -> enn6 vm
    | 55 -> (match f with
        | 0 -> incx vm)
        | 1 -> decx vm
        | 2 -> entx vm
        | 3 -> ennx vm
    | 56 -> cmpa vm
    | 57 -> cmp1 vm
    | 58 -> cmp2 vm
    | 59 -> cmp3 vm
    | 60 -> cmp4 vm
    | 61 -> cmp5 vm
    | 62 -> cmp6 vm
    | 63 -> cmpx vm

let rec step vm =
  dispatch vm (fetch vm)
;;
