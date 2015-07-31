type vm = {
  mem : Mixword.t array;
  rI : MixWord.t array;
  rA : MixWord.t;
  rX : MixWord.t;
  rJ : MixWord.t;

  mutable xC : int;
  mutable xF : int;
  mutable xI : int;
  mutable xA : int;
  mutable xM : int;

  mutable loc : int;
  mutable comparison : int;
  mutable overflow : bool;
  mutable halted : bool;
  mutable tracing : bool;
}

let make_vm () = {
  mem = Array.make 4000 Mixword.zero;

  rI = Array.make 7 Mixword.zero;
  rA = Mixword.zero;
  rX = Mixword.zero;
  rJ = Mixword.zero;

  xC = 0;
  xF = 0;
  xI = 0;
  xA = 0;
  xM = 0;

  loc = 0;
  comparison = 0;
  overflow = false;
  halted = true;
  tracing = false;
}

let fetch vm =
  let lc = vm.loc in
    vm.loc <- vm.loc + 1;
    let inst = vm.mem.[lc] in
    vm.xC <- Mixword.getfieldi inst 5 5;
    vm.xF <- Mixword.getfieldi inst 4 4;
    vm.xI <- Mixword.getfieldi inst 3 3;
    vm.xA <- Mixword.getfieldi inst 0 2;
    vm.xM <- (Mixword.to_int vm.rI.[vm.xI]) + vm.xA

let step vm =
  fetch vm;
  Opcode.dispatch vm.xC vm

let run vm =
  vm.halted <- false
  while not vm.halted do
    step vm
  done
