%{
  open Printf
  open Pcode
  open Error
%}

%token <float> T_FLOAT
%token <string> T_STRING
%token <string> T_FLOATVAR T_STRINGVAR
%token T_GOSUB T_RETURN T_PRINT T_GOTO T_END T_COMMA
%token T_LP T_RP T_PLUS T_EQ T_EOL T_LET T_MINUS T_STAR
%token T_LIST T_RUN T_EXIT T_IF T_THEN T_NEXT T_FOR T_TO
%token T_STEP T_DIM T_CALL T_SEMI T_OR T_AND T_NEQ
%token T_GE T_LE T_GT T_LT T_CARET T_SLASH T_INPUT
%token T_STOP T_DATA T_READ T_DEL T_LOAD T_SAVE
%token T_MOD
%token <string> T_REM

%left T_AND T_OR
%left T_EQ T_NEQ T_GE T_LE T_GT T_LT
%left T_PLUS T_MINUS
%left T_STAR T_SLASH
%left T_MOD
%left T_CARET

%type <int> int
%type <Pcode.exp> exp
%type <Pcode.cmd> cmd
%type <Pcode.directive> line

%start line

%%
int: T_FLOAT
  { let n = int_of_float $1 in
    if float_of_int n = $1 then n
    else $syntaxerror }
;
line:
  int cmd? T_EOL
  { match $2 with
    | None -> DEL $1
    | Some a -> STORE ($1,a) }
  | directive T_EOL
  { $1 }
  | T_EOL
  { NIL }
;
print_exp: s = separated_list(T_SEMI,exp)
  { List.tl (List.flatten (List.map (fun x -> [NOSPACE;EXP x]) s)) }
;
dim_expr:
  v = var; T_LP; s = separated_nonempty_list(T_COMMA,exp); T_RP
  { (v, s) }
;
prompt:
  T_STRING T_COMMA
  { $1 }
;
input_expr:
  prompt? T_STRINGVAR
  { INPUT ($1, LINE $2) }
  | p = prompt?; s = separated_nonempty_list(T_COMMA,T_FLOATVAR)
  { INPUT (p, FLOATS s) }
;
directive:
  | T_LOAD T_STRING
  { LOAD $2 }
  | T_DEL int
  { DEL $2 }
  | T_RUN int?
  { RUN $2 }
  | T_LIST
  { LIST }
  | T_EXIT
  { EXIT }
  | T_SAVE T_STRING
  { SAVE $2 }
;
cmd:
  | T_READ; s = separated_nonempty_list(T_COMMA,var)
  { READ s }
  | T_DATA; s = separated_nonempty_list(T_COMMA,exp)
  { DATA s }
  | T_STOP
  { STOP }
  | T_INPUT input_expr
  { $2 }
  | T_REM
  { REM $1 }
  | T_DIM; s = separated_nonempty_list(T_COMMA, dim_expr)
  { DIM s }
  | T_IF exp T_THEN cmd
  { IF ($2, $4) }
  | T_IF exp T_THEN int
  { IF ($2, GOTO $4) }
  | T_FOR T_FLOATVAR T_EQ exp T_TO exp T_STEP exp
  { FOR ($2, $4, $6, Some $8) }
  | T_FOR T_FLOATVAR T_EQ exp T_TO exp
  { FOR ($2, $4, $6, None) }
  | T_PRINT print_exp
  { PRINT $2 }
  | T_GOSUB int
  { GOSUB $2 }
  | T_RETURN
  { RETURN }
  | T_GOTO int
  { GOTO $2 }
  | T_END
  { END }
  | T_LET ref T_EQ exp
  { LET ($2, $4) }
  | r = ref; T_EQ; x = exp
  { LET (r, x) }
  | ref
  { match $1 with
    | VAR v -> CALL (v,[])
    | SUBSCRIPT (v,el) -> CALL (v,el) }
  | T_NEXT T_FLOATVAR?
  { NEXT $2 }
  | T_CALL; v = var; s = separated_list(T_COMMA,exp)
  { CALL (v,s) }
;

var:
  T_STRINGVAR
  { STRINGVAR $1 }
  | T_FLOATVAR
  { FLOATVAR $1 }
;

ref:
  var
  { VAR $1 }
  | r = var T_LP; s = separated_nonempty_list(T_COMMA,exp); T_RP
  { SUBSCRIPT (r, s) }
;

exp:
  T_FLOAT
  { FLOAT $1 }
  | T_STRING
  { STRING $1 }
  | exp T_PLUS exp
  { BIN ($1, ADD, $3) }
  | exp T_OR exp
  { BIN ($1, OR, $3) }
  | exp T_AND exp
  { BIN ($1, AND, $3) }
  | exp T_MINUS exp
  { BIN ($1, SUB, $3) }
  | exp T_STAR exp
  { BIN ($1, MUL, $3) }
  | T_LP exp T_RP
  { $2 }
  | exp T_EQ exp
  { BIN ($1, EQ, $3) }
  | exp T_NEQ exp
  { BIN ($1, NEQ, $3) }
  | exp T_GT exp
  { BIN ($1, GT, $3) }
  | exp T_LT exp
  { BIN ($1, LT, $3) }
  | exp T_GE exp
  { BIN ($1, GE, $3) }
  | exp T_SLASH exp
  { BIN ($1, DIV, $3) }
  | exp T_LE exp
  { BIN ($1, LE, $3) }
  | exp T_CARET exp
  { BIN ($1, POW, $3) }
  | ref
  { REF $1 }
  | exp T_MOD exp
  { BIN ($1, MOD, $3) }
;
