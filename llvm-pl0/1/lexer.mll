{
  open Parser

  let keywords =
    let tbl = Hashtbl.create 10 in
      List.iter (fun (x, y) -> Hashtbl.add tbl x y)
        [
          "const", CONST;
          "var", VAR;
          "call", CALL;
          "procedure", PROCEDURE;
          "if", IF;
          "then", THEN;
          "while", WHILE;
          "do", DO;
          "odd", ODD;
          "begin", BEGIN;
          "end", END
        ];
      tbl
}

rule token = parse
  | ['\n' ' ' '\t']+
  {
    token lexbuf
  }
  | ['a'-'z''A'-'Z']+ as ident
  {
    try Hashtbl.find keywords (String.lowercase ident)
    with Not_found -> IDENT ident
  }
  | ['0'-'9']+ as number
  {
    INT (int_of_string number)
  }
  | ":="
  {
    COLONEQ
  }
  | '.'
  {
    DOT
  }
  | ','
  {
    COMMA
  }
  | ';'
  {
    SEMI
  }
  | '('
  {
    LP
  }
  | ')'
  {
    RP
  }
  | '*'
  {
    STAR
  }
  | '/'
  {
    SLASH
  }
  | '+'
  {
    PLUS
  }
  | '-'
  {
    MINUS
  }
  | "<>"
  {
    NEQ
  }
  | '='
  {
    EQ
  }
  | ">="
  {
    GE
  }
  | '>'
  {
    GT
  }
  | "<="
  {
    LE
  }
  | '<'
  {
    LT
  }
  | eof
  {
    EOF
  }
