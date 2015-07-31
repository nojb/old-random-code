let main () =
  let lexbuf = Lexing.from_channel stdin in
  let p = Parser.program Lexer.token lexbuf in
  let s = Semant.program p in
  let s = Lift.program s in
  let m = Codegen.program s in
    Llvm.dump_module m;
    Llvm.dispose_module m
;;

main ()
