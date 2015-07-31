let main () =
  let lexbuf = Lexing.from_channel stdin in
  let p = Parser.program Lexer.token lexbuf in
  print_endline "[parser] done";
  let s = Semant.program p in
  print_endline "[semant] done";
  let s = Lift.program s in
  print_endline "[lift] done";
  let m = Codegen.program s in
    print_endline "[codegen] done";
    Llvm.dump_module m
;;

main ()
