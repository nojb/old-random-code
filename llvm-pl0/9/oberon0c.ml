let main () =
  let lexbuf = Lexing.from_channel stdin in
  let p = Parser.oberon_module Lexer.token lexbuf in
  let s = Semant.oberon_module p in
  let s = Lift.oberon_module s in
  let m = Codegen.oberon_module s in
    Llvm.dump_module m;
    Llvm.dispose_module m
;;

main ()
