open Word

type vm = {
  mutable rA : word;
  mutable rX : word;
  mutable rI : short array;
  mutable rJ : short;
  mutable overflow : bool;
  mutable comparison : int;
  memory : word array;
  mutable pc : int;
  mutable time : int;
  mutable stopped : bool;
}

let dump vm =
  Printf.printf "rI1 = %s\trA = %s\nrI2 = %s\trX = %s\nrI3 = %s\trJ = %s\nrI4 = %s\n"
    (string_of_short vm.rI.(1))
    (string_of_word vm.rA)
    (string_of_short vm.rI.(2))
    (string_of_word vm.rX)
    (string_of_short vm.rI.(3))
    (string_of_short vm.rJ)
    (string_of_short vm.rI.(4));
  Printf.printf "rI5 = %s\nrI6 = %s\n"
    (string_of_short vm.rI.(5))
    (string_of_short vm.rI.(6));
  Printf.printf "comparison = %d\t\toverflow = %b\n" vm.comparison vm.overflow;
  Printf.printf "        pc = %04d\t stopped = %b\n" vm.pc vm.stopped;
  Printf.printf "      time = %d\n" vm.time

let make () = {
  rA = word_zero ();
  rX = word_zero ();
  rI = Array.init 7 (fun _ -> short_zero ());
  rJ = short_zero ();
  overflow = false;
  comparison = 0;
  memory = Array.init 4000 (fun _ -> word_zero ());
  pc = 0;
  time = 0;
  stopped = true;
}

let step vm =
  let inst = vm.memory.(vm.pc) in
  let c = inst.(5) in
  let f = inst.(4) in
  let i = inst.(3) in
  let aa = int_of_field inst 0 2 in
  let m = aa + int_of_short vm.rI.(i) in
  Printf.printf
    "executing %s @ %d: opcode %d \
      (aa = %d, m = %d, f = %d, i = %d)\n"
    (string_of_word inst) vm.pc c aa m f i;
  vm.pc <- vm.pc + 1;
  (match c with
  | 0 (* NOP *) -> begin (* should f be ignored? *)
    vm.time <- vm.time + 1
  end
  | 8 (* LDA *) -> begin
    vm.time <- vm.time + 2;
    vm.rA <- word_field vm.memory.(m) (f / 8) (f mod 8)
  end
  | 15 (* LDX *) -> begin
    vm.time <- vm.time + 2;
    vm.rX <- word_field vm.memory.(m) (f / 8) (f mod 8)
  end
  | 9 | 10 | 11 | 12 | 13 | 14 (* LDi *) -> begin
    vm.time <- vm.time + 2;
    vm.rI.(c-8) <- word_field vm.memory.(m) (f / 8) (f mod 8)
  end
  | 16 (* LDAN *) -> begin
    vm.time <- vm.time + 2;
    vm.rA <- word_neg (word_field vm.memory.(m) (f / 8) (f mod 8))
  end
  | 23 (* LDXN *) -> begin
    vm.time <- vm.time + 2;
    vm.rX <- word_neg (word_field vm.memory.(m) (f / 8) (f mod 8))
  end
  | 17 | 18 | 19 | 20 | 21 | 22 (* LDiN *) -> begin
    vm.time <- vm.time + 2;
    vm.rI.(c-16) <- word_neg (word_field vm.memory.(m) (f / 8) (f mod 8))
  end
  | 24 (* STA *) -> begin
    vm.time <- vm.time + 2;
    word_store vm.memory.(m) vm.rA (f / 8) (f mod 8)
  end
  | 31 (* STX *) -> begin
    vm.time <- vm.time + 2;
    word_store vm.memory.(m) vm.rX (f / 8) (f mod 8)
  end
  | 25 | 26 | 27 | 28 | 29 | 30 (* STi *) -> begin
    vm.time <- vm.time + 2;
    word_store vm.memory.(m) vm.rI.(c-24) (f / 8) (f mod 8)
  end
  | 32 (* STJ *) -> begin
    vm.time <- vm.time + 2;
    word_store vm.memory.(m) (word_of_short vm.rJ) (f / 8) (f mod 8)
    (* should make sure that the sign of rJ is always + *)
  end
  | 33 (* STZ *) -> begin
    vm.time <- vm.time + 2;
    word_store vm.memory.(m) (word_zero ()) (f / 8) (f mod 8)
  end
  | 1 (* ADD *) -> begin
    vm.time <- vm.time + 2;
    let v = word_field vm.memory.(m) (f / 8) (f mod 8) in
    let carry = ref false in
      vm.rA <- word_add vm.rA v carry;
      vm.overflow <- !carry
  end
  | 2 (* SUB *) -> begin
    vm.time <- vm.time + 2;
    let v = word_field vm.memory.(m) (f / 8) (f mod 8) in
    let carry = ref false in
      vm.rA <- word_add vm.rA (word_neg v) carry;
      vm.overflow <- !carry
  end
  | 48 when f = 2 (* ENTA *) -> begin
    vm.time <- vm.time + 1;
    if m = 0 then
      vm.rA <- [|inst.(0); 0; 0; 0; 0; 0|]
    else
      vm.rA <- word_of_int m
  end
  | 55 when f = 2 (* ENTX *) -> begin
    vm.time <- vm.time + 1;
    if m = 0 then
      vm.rX <- [|inst.(0); 0; 0; 0; 0; 0|]
    else
      vm.rX <- word_of_int m
  end
  | 49 | 50 | 51 | 52 | 53 | 54 when f = 2 (* ENTi *) -> begin
    vm.time <- vm.time + 1;
    if m = 0 then
      vm.rI.(c-48) <- [|inst.(0); 0; 0|]
    else
      vm.rI.(c-48) <- short_of_int m
  end
  | 48 when f = 3 (* ENNA *) -> begin
    vm.time <- vm.time + 1;
    if m = 0 then
      vm.rA <- [|-inst.(0); 0; 0; 0; 0; 0|]
    else
      vm.rA <- word_of_int (-m)
  end
  | 55 when f = 3 (* ENNX *) -> begin
    vm.time <- vm.time + 1;
    if m = 0 then
      vm.rX <- [|-inst.(0); 0; 0; 0; 0; 0|]
    else
      vm.rX <- word_of_int (-m)
  end
  | 49 | 50 | 51 | 52 | 54 when f = 3 (* ENNi *) -> begin
    vm.time <- vm.time + 1;
    if m = 0 then
      vm.rI.(c-48) <- [|-inst.(0); 0; 0|]
    else
      vm.rI.(c-48) <- short_of_int (-m)
  end
  | 48 when f = 0 (* INCA *) -> begin
    vm.time <- vm.time + 1;
    let carry = ref false in
      vm.rA <- word_add vm.rA (word_of_int m) carry;
      vm.overflow <- !carry
  end
  | 55 when f = 0 (* INCX *) -> begin
    vm.time <- vm.time + 1;
    let carry = ref false in
      vm.rX <- word_add vm.rX (word_of_int m) carry;
      vm.overflow <- !carry
  end
  | 49 | 50 | 51 | 52 | 53 | 54 when f = 0 (* INCi *) -> begin
    vm.time <- vm.time + 1;
    let carry = ref false in
      vm.rI.(c-48) <-
        short_of_word (word_add (word_of_short (vm.rI.(c-48))) (word_of_int m)
          carry) (* if overflow occus, result is undefined *)
  end
  | 48 when f = 1 (* DECA *) -> begin
    vm.time <- vm.time + 1;
    let carry = ref false in
      vm.rA <- word_add vm.rA (word_of_int (-m)) carry;
      vm.overflow <- !carry
  end
  | 55 when f = 1 (* DECX *) -> begin
    vm.time <- vm.time + 1;
    let carry = ref false in
      vm.rX <- word_add vm.rX (word_of_int (-m)) carry;
      vm.overflow <- !carry
  end
  | 49 | 50 | 51 | 52 | 53 | 54 when f = 1 (* DECi *) -> begin
    vm.time <- vm.time + 1;
    let carry = ref false in
      vm.rI.(c-48) <-
        short_of_word (word_add (word_of_short (vm.rI.(c-48))) (word_of_int
        (-m)) carry)
  end
  | 56 (* CMPA *) -> begin
    vm.time <- vm.time + 2;
    let l = f / 8 in
    let r = f mod 8 in
      vm.comparison <-
        compare (word_field vm.rA l r) (word_field vm.memory.(m) l r)
  end
  | 63 (* CMPX *) -> begin
    vm.time <- vm.time + 2;
    let l = f / 8 in
    let r = f mod 8 in
      vm.comparison <-
        compare (word_field vm.rX l r) (word_field vm.memory.(m) l r)
  end
  | 57 | 58 | 59 | 60 | 61 | 62 (* CMPi *) -> begin
    vm.time <- vm.time + 2;
    let l = f / 8 in
    let r = f mod 8 in
      vm.comparison <-
        compare (word_field vm.rI.(c-56) l r) (word_field vm.memory.(m) l r)
  end
  | 39 when f = 0 (* JMP *) -> begin
    vm.time <- vm.time + 1;
    vm.rJ <- short_of_int vm.pc;
    vm.pc <- m
  end
  | 39 when f = 1 (* JSJ *) -> begin
    vm.time <- vm.time + 1;
    vm.pc <- m
  end
  | 39 when f = 2 (* JOV *) -> begin
    vm.time <- vm.time + 1;
    if vm.overflow then begin
      vm.overflow <- false;
      vm.rJ <- short_of_int vm.pc;
      vm.pc <- m
    end
  end
  | 39 when f = 3 (* JNOV *) -> begin
    vm.time <- vm.time + 1;
    if vm.overflow then
      vm.overflow <- false
    else begin
      vm.rJ <- short_of_int vm.pc;
      vm.pc <- m
    end
  end
  | 39 when f = 4 (* JL *) -> begin
    vm.time <- vm.time + 1;
    if vm.comparison < 0 then begin
      vm.rJ <- short_of_int vm.pc;
      vm.pc <- m
    end
  end
  | 39 when f = 5 (* JE *) -> begin
    vm.time <- vm.time + 1;
    if vm.comparison = 0 then begin
      vm.rJ <- short_of_int vm.pc;
      vm.pc <- m
    end
  end
  | 39 when f = 6 (* JG *) -> begin
    vm.time <- vm.time + 1;
    if vm.comparison > 0 then begin
      vm.rJ <- short_of_int vm.pc;
      vm.pc <- m
    end
  end
  | 39 when f = 7 (* JGE *) -> begin
    vm.time <- vm.time + 1;
    if vm.comparison >= 0 then begin
      vm.rJ <- short_of_int vm.pc;
      vm.pc <- m
    end
  end
  | 39 when f = 8 (* JNE *) -> begin
    vm.time <- vm.time + 1;
    if vm.comparison <> 0 then begin
      vm.rJ <- short_of_int vm.pc;
      vm.pc <- m
    end
  end
  | 39 when f = 9 (* JLE *) -> begin
    vm.time <- vm.time + 1;
    if vm.comparison <= 0 then begin
      vm.rJ <- short_of_int vm.pc;
      vm.pc <- m
    end
  end
  | 40 when f = 0 (* JAN *) -> begin
    vm.time <- vm.time + 1;
    if int_of_word vm.rA < 0 then begin
      vm.rJ <- short_of_int vm.pc;
      vm.pc <- m
    end
  end
  | 40 when f = 1 (* JAZ *) -> begin
    vm.time <- vm.time + 1;
    if int_of_word vm.rA = 0 then begin
      vm.rJ <- short_of_int vm.pc;
      vm.pc <- m
    end
  end
  | 40 when f = 2 (* JAP *) -> begin
    vm.time <- vm.time + 1;
    if int_of_word vm.rA > 0 then begin
      vm.rJ <- short_of_int vm.pc;
      vm.pc <- m
    end
  end
  | 40 when f = 3 (* JANN *) -> begin
    vm.time <- vm.time + 1;
    if int_of_word vm.rA >= 0 then begin
      vm.rJ <- short_of_int vm.pc;
      vm.pc <- m
    end
  end
  | 40 when f = 4 (* JANZ *) -> begin
    vm.time <- vm.time + 1;
    if int_of_word vm.rA <> 0 then begin
      vm.rJ <- short_of_int vm.pc;
      vm.pc <- m
    end
  end
  | 40 when f = 5 (* JANP *) -> begin
    vm.time <- vm.time + 1;
    if int_of_word vm.rA <= 0 then begin
      vm.rJ <- short_of_int vm.pc;
      vm.pc <- m
    end
  end
  | 47 when f = 0 (* JXN *) -> begin
    vm.time <- vm.time + 1;
    if int_of_word vm.rX < 0 then begin
      vm.rJ <- short_of_int vm.pc;
      vm.pc <- m
    end
  end
  | 47 when f = 1 (* JXZ *) -> begin
    vm.time <- vm.time + 1;
    if int_of_word vm.rX = 0 then begin
      vm.rJ <- short_of_int vm.pc;
      vm.pc <- m
    end
  end
  | 47 when f = 2 (* JXP *) -> begin
    vm.time <- vm.time + 1;
    if int_of_word vm.rX > 0 then begin
      vm.rJ <- short_of_int vm.pc;
      vm.pc <- m
    end
  end
  | 47 when f = 3 (* JXNN *) -> begin
    vm.time <- vm.time + 1;
    if int_of_word vm.rX >= 0 then begin
      vm.rJ <- short_of_int vm.pc;
      vm.pc <- m
    end
  end
  | 47 when f = 4 (* JXNZ *) -> begin
    vm.time <- vm.time + 1;
    if int_of_word vm.rX <> 0 then begin
      vm.rJ <- short_of_int vm.pc;
      vm.pc <- m
    end
  end
  | 47 when f = 5 (* JXNP *) -> begin
    vm.time <- vm.time + 1;
    if int_of_word vm.rX <= 0 then begin
      vm.rJ <- short_of_int vm.pc;
      vm.pc <- m
    end
  end
  | 41 | 42 | 43 | 44 | 45 | 46 when f = 0 (* JiN *) -> begin
    vm.time <- vm.time + 1;
    if int_of_short vm.rI.(c-40) < 0 then begin
      vm.rJ <- short_of_int vm.pc;
      vm.pc <- m
    end
  end
  | 41 | 42 | 43 | 44 | 45 | 46 when f = 1 (* JiZ *) -> begin
    vm.time <- vm.time + 1;
    if int_of_short vm.rI.(c-40) = 0 then begin
      vm.rJ <- short_of_int vm.pc;
      vm.pc <- m
    end
  end
  | 41 | 42 | 43 | 44 | 45 | 46 when f = 2 (* JiP *) -> begin
    vm.time <- vm.time + 1;
    if int_of_short vm.rI.(c-40) > 0 then begin
      vm.rJ <- short_of_int vm.pc;
      vm.pc <- m
    end
  end
  | 41 | 42 | 43 | 44 | 45 | 46 when f = 3 (* JiNN *) -> begin
    vm.time <- vm.time + 1;
    if int_of_short vm.rI.(c-40) >= 0 then begin
      vm.rJ <- short_of_int vm.pc;
      vm.pc <- m
    end
  end
  | 41 | 42 | 43 | 44 | 45 | 46 when f = 4 (* JiNZ *) -> begin
    vm.time <- vm.time + 1;
    if int_of_short vm.rI.(c-40) <> 0 then begin
      vm.rJ <- short_of_int vm.pc;
      vm.pc <- m
    end
  end
  | 41 | 42 | 43 | 44 | 45 | 46 when f = 5 (* JiNP *) -> begin
    vm.time <- vm.time + 1;
    if int_of_short vm.rI.(c-40) <= 0 then begin
      vm.rJ <- short_of_int vm.pc;
      vm.pc <- m
    end
  end
  | 7 (* MOVE *) -> begin
    vm.time <- vm.time + 2*f;
    let pos = int_of_short vm.rI.(1) in
      for i = 0 to f-1 do
        vm.memory.(pos+i) <- vm.memory.(m+i)
      done;
      vm.rI.(1) <- short_of_int (pos+f)
  end
  | 5 when f = 2 (* HLT *) -> begin
    vm.time <- vm.time + 10;
    vm.stopped <- true
  end
  | 5 when f = 1 (* CHAR *) -> begin
    vm.time <- vm.time + 10;
    let n = int_of_word vm.rA in
    let s = Printf.sprintf "%010d" n in
    for i = 1 to 5 do
      vm.rA.(i) <- (int_of_char s.[i-1])-18;
      vm.rX.(i) <- (int_of_char s.[i+4])-18
    done
  end
  | _ -> begin
    failwith "not implemented"
  end); dump vm

let run vm orig =
  vm.stopped <- false;
  vm.pc <- orig;
  while not vm.stopped do
    step vm
  done;
  print_endline "machine halted.";
  dump vm
