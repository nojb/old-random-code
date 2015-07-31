let main () =
  let lexbuf = Lexing.from_channel stdin in
  let p = Parser.program Lexer.token lexbuf in
    Interp.interp_program p
;;

main ()
