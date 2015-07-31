{
  open Parser

  let keywords =
    let tbl = Hashtbl.create 10 in
      List.iter (fun (x, y) -> Hashtbl.add tbl x y)
        [
          "div", DIV;
          "or", OR;
          "const", CONST;
          "var", VAR;
          "procedure", PROCEDURE;
          "if", IF;
          "then", THEN;
          "while", WHILE;
          "do", DO;
          "begin", BEGIN;
          "end", END;
          "module", MODULE;
          "mod", MOD;
          "else", ELSE;
          "elsif", ELSIF
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
