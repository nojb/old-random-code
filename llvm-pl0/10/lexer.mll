{
  open Parser

  let keywords =
    let tbl = Hashtbl.create 10 in
      List.iter (fun (x, y) -> Hashtbl.add tbl x y)
        [
          "OF", OF;
          "DIV", DIV;
          "OR", OR;
          "CONST", CONST;
          "VAR", VAR;
          "PROCEDURE", PROCEDURE;
          "IF", IF;
          "THEN", THEN;
          "WHILE", WHILE;
          "DO", DO;
          "BEGIN", BEGIN;
          "END", END;
          "MODULE", MODULE;
          "MOD", MOD;
          "ELSE", ELSE;
          "ELSIF", ELSIF;
          "ARRAY", ARRAY;
          "RECORD", RECORD;
          "TYPE", TYPE
        ];
      tbl
}

rule token = parse
  | ['\n' ' ' '\t']+
  {
    token lexbuf
  }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as ident
  {
    try Hashtbl.find keywords (String.uppercase ident)
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
  | ':'
  {
    COLON
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
  | '['
  {
    LB
  }
  | ']'
  {
    RB
  }
  | '('
  {
    LP
  }
  | ')'
  {
    RP
  }
  | '&'
  {
    AMPERSAND
  }
  | '~'
  {
    TILDE
  }
  | '*'
  {
    STAR
  }
  | '+'
  {
    PLUS
  }
  | '-'
  {
    MINUS
  }
  | '#'
  {
    SHARP
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
