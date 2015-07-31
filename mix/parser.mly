%{
open Opcodes
open Globals
%}
%token <int> NUMBER
%token <string> SYMBOL
%token LPAR RPAR EQ EOL STAR COMMA
%token PLUS MINUS TIMES DIV DIVDIV COLON
%type <int> exp
%type <int> word
%type <int * int * int option> addr
%start exp
%start word
%start addr
%%

atomic_exp:
	| n = NUMBER { n }
	| STAR { curr_loc () }
	| s = SYMBOL { find_sym s }
;

exp1:
	| e = atomic_exp { e }
	| PLUS e = atomic_exp { e }
	| MINUS e = atomic_exp { -e }
	| e1 = exp1 op = binop e2 = atomic_exp { op e1 e2 }
;

binop:
	| PLUS { ( + ) }
	| MINUS { ( - ) }
	| TIMES { ( * ) }
	| DIV { ( / ) }
	| DIVDIV { fun x y -> failwith "I don't know what DIVDIV is!" }
	| COLON { fun x y -> 8*x+y }
;

addr1:
	| { 0 }
	| e = exp1 { e }
	| EQ x = word1 EQ { add_literal x }
;

word1:
	| l = separated_nonempty_list(COMMA,pair(exp1,ioption(delimited(LPAR,exp1,RPAR))))
	{
		List.fold_left (fun x (e,f) -> Words.set_field x (match f with None -> 5 | Some u -> u) e) 0 l
	}
;

exp:
	| e = exp1 EOL { e }
;

word:
	| w = word1 EOL { w }
;

addr:
	| a = addr1 i = ioption(preceded(COMMA,exp1)) f = ioption(delimited(LPAR,exp1,RPAR)) EOL { (a, (match i with None -> 0 | Some x -> x), f) }
;
