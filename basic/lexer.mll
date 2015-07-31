{
  open Printf
  open Parser

  let str_buf = Buffer.create 100

  let setup_keywords lst =
    let tbl = Hashtbl.create (List.length lst) in
    List.iter (fun (x,y) -> Hashtbl.add tbl x y) lst;
    tbl
  
  let keywords = setup_keywords [
    "MOD", T_MOD;
    "SAVE", T_SAVE;
    "LOAD", T_LOAD;
    "DEL", T_DEL;
    "READ", T_READ;
    "DATA", T_DATA;
    "STOP", T_STOP;
    "INPUT", T_INPUT;
    "GOSUB", T_GOSUB;
    "DIM", T_DIM;
    "RETURN", T_RETURN;
    "GOTO", T_GOTO;
    "LET", T_LET;
    "IF", T_IF;
    "THEN", T_THEN;
    "PRINT", T_PRINT;
    "LIST", T_LIST;
    "END", T_END;
    "RUN", T_RUN;
    "EXIT", T_EXIT;
    "NEXT", T_NEXT;
    "CALL", T_CALL;
    "FOR", T_FOR;
    "TO", T_TO;
    "STEP", T_STEP;
    "AND", T_AND;
    "OR", T_OR
  ]
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let ident = letter (digit|letter|'_')*

rule token = parse
  | ";" { T_SEMI }
  | "<>" { T_NEQ }
  | "<=" { T_LE }
  | ">=" { T_GE }
  | "<" { T_LT }
  | ">" { T_GT } 
  | "=" { T_EQ }
  | "+" { T_PLUS }
  | "*" { T_STAR }
  | "-" { T_MINUS }
  | "(" { T_LP }
  | ")" { T_RP }
  | "," { T_COMMA }
  | "/" { T_SLASH }
  | ['R' 'r'] ['E' 'e'] ['M' 'm'] ([' ' '\t'] (_)*)? {
    let s = Lexing.lexeme lexbuf in
    T_REM (String.sub s 3 ((String.length s)-3))
  }
  | "^" { T_CARET }
  | "\"" { Buffer.clear str_buf; str lexbuf }
  | (("+"|"-")?digit+ ('.' digit+)?) as i { T_FLOAT (float_of_string i) }
  | (ident as id) '$' { T_STRINGVAR (String.uppercase id) }
  | ident as id {
    let up = String.uppercase id in
      try Hashtbl.find keywords up
      with Not_found -> T_FLOATVAR up
  }
  | [' ' '\t'] { token lexbuf }
  | eof { T_EOL }

and str = parse
  | "\"" { T_STRING (Buffer.contents str_buf) }
  | _ as c { Buffer.add_char str_buf c; str lexbuf }

{
  let tokenize_string s f =
    let lexbuf = Lexing.from_string s in
    let rec next t =
      f t;
      if t != T_EOL then next (token lexbuf)
    in next (token lexbuf)
}
