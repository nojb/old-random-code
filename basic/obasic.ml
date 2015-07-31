open Printf
open Pcode
open Interp

let load_channel vm ic =
  try while true do
    let line = input_line ic in
    let lexbuf = Lexing.from_string line in
    try match Parser.line Lexer.token lexbuf with
    | STORE (i,c) -> store_line vm i c
    | _ -> ()
    with Parser.Error -> begin
      printf "SYNTAX ERROR WHILE LOADING"
    end
  done with End_of_file -> ()

let today () =
  let month_name i =
    match i with
    | 0 -> "JANUARY" | 1 -> "FEBRUARY"
    | 2 -> "MARCH" | 3 -> "APRIL"
    | 4 -> "MAY" | 5 -> "JUNE"
    | 6 -> "JULY" | 7 -> "AUGUST"
    | 8 -> "SEPTEMBER" | 9 -> "OCTOBER"
    | 10 -> "NOVEMBER" | 11 -> "DECEMBER"
    | _ -> assert false
  in let tm = Unix.localtime (Unix.time ()) in
    sprintf "%2d %s %d\t\tTIME: %02d:%02d"
      tm.Unix.tm_mday
      (month_name tm.Unix.tm_mon)
      (1900 + tm.Unix.tm_year)
      tm.Unix.tm_hour
      tm.Unix.tm_min

let profile name f =
  printf "PROBLEM NAME: %s\t\t%s\n"
    (if name = "" then "<EMPTY>" else name)
    (today ());
  let start_time = Unix.time () in
  f ();
  let end_time = Unix.time () in
  printf "TIME: %f SECS.\n" ((end_time -. start_time));
  flush stdout

let eval_directive vm dir =
  match dir with
  | EXIT -> raise Exit
  | LIST -> list_program stdout vm
  | RUN None -> profile vm.name (fun () -> run vm 0)
  | RUN (Some x) ->
    profile vm.name
      (fun () -> run vm (find_label vm x))
  | LOAD s -> load_channel vm (open_in s)
  | STORE (i,c) -> store_line vm i c
  | DEL i -> delete_line vm i
  | SAVE s -> begin
    let oc = open_out s in
    DynArray.iter
      (fun (i,c) -> fprintf oc "%d %s\n" i (string_of_cmd c))
      vm.program
  end
  | NIL -> ()

let main () =
  let vm = make_vm () in
  Stdlib.register vm;
  ignore (Sys.command "clear");
  print_endline "WELCOME TO OCAML-BASIC - 2009";
  try while true do
    print_string "> "; flush stdout;
    let line = read_line () in
    let lexbuf = Lexing.from_string line in
      try eval_directive vm (Parser.line Lexer.token lexbuf)
      with
      | Parser.Error -> begin
      (*  let p = lexbuf.Lexing.lex_curr_p in
          for i = 1 to p.Lexing.pos_cnum do
            print_char ' '
          done;
          print_endline "^";*)
          print_endline "ERROR Syntax error"
      end
      | Error.ERROR desc -> Error.fail_explain desc
      | e -> raise e
    done
  with Exit -> print_endline "BYE"

let _ = main ()
