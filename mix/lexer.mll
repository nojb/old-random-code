{
open Parser
}
rule token = parse
	  [' ' '\t']+ { token lexbuf }
	| '+' { PLUS }
	| ',' { COMMA }
	| ':' { COLON }
	| '-' { MINUS }
	| '*' { STAR }
	| '/' { DIV }
	| '=' { EQ }
	| "//" { DIVDIV }
	| '(' { LPAR }
	| ')' { RPAR }
	| ['0'-'9']+ as lxm { NUMBER(int_of_string lxm) }
	| ['0'-'9' 'a'-'z' 'A'-'Z']+ as lxm { SYMBOL(lxm) }
	| eof { EOL }
