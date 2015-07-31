%token DOT EOF CONST COMMA SEMI VAR EQ
%token PROCEDURE IF THEN WHILE DO
%token BEGIN END ODD LP RP LT LE GT GE
%token PLUS MINUS STAR SLASH NEQ COLONEQ

%token <int> INT
%token <string> IDENT

%left PLUS MINUS
%left STAR SLASH
%left UMINUS

%type <Ast.block_info> program
%start program

%%

program:
  block DOT EOF
  {
    $1
  }
  ;

block:
  const_part var_part list(proc_part) statement
  {
    {
      Ast.block_constants = $1;
      Ast.block_variables = $2;
      Ast.block_procedures = $3;
      Ast.block_body = $4;
    }
  }
  ;

const_part:
  {
    []
  }
  | CONST separated_nonempty_list(COMMA,const_dec) SEMI
  {
    $2
  }
;

const_dec:
  IDENT EQ INT
  {
    ($1, $3)
  }
  ;

var_part:
  {
    []
  }
  | VAR separated_nonempty_list(COMMA,IDENT) SEMI
  {
    $2
  }
  ;

proc_part:
  PROCEDURE name = IDENT params =
    loption(delimited(LP,separated_nonempty_list(COMMA,pair(boption(VAR),IDENT)),RP)) SEMI body = block SEMI
  {
    {
      Ast.procedure_name = name;
      Ast.procedure_parameters = params;
      Ast.procedure_block = body;
    }
  }
  ;

statement:
  IDENT COLONEQ expression
  {
    Ast.Assign ($1, $3)
  }
  | name = IDENT args = loption(delimited(LP,separated_nonempty_list(COMMA,expression),RP))
  {
    Ast.Call (name, args)
  }
  | BEGIN separated_nonempty_list(SEMI,statement) END
  {
    Ast.Begin ($2)
  }
  | IF condition THEN statement
  {
    Ast.If ($2, $4)
  }
  | WHILE condition DO statement
  {
    Ast.While ($2, $4)
  }
  ;

condition:
  ODD expression
  {
    Ast.Odd ($2)
  }
  | e1 = expression op = relop e2 = expression
  {
    Ast.Rel (e1, op, e2)
  }
  ;

%inline relop:
  EQ
  {
    Ast.Eq
  }
  | NEQ
  {
    Ast.Neq
  }
  | LT
  {
    Ast.Lt
  }
  | LE
  {
    Ast.Le
  }
  | GE
  {
    Ast.Ge
  }
  | GT
  {
    Ast.Gt
  }
  ;

%inline binop:
  PLUS
  {
    Ast.Add
  }
  | MINUS
  {
    Ast.Sub
  }
  | STAR
  {
    Ast.Mul
  }
  | SLASH
  {
    Ast.Div
  }
  ;

%inline unop:
  PLUS
  {
    Ast.Plus
  }
  | MINUS
  {
    Ast.Minus
  }
  ;

expression:
  IDENT
  {
    Ast.Var ($1)
  }
  | INT
  {
    Ast.Int ($1)
  }
  | LP expression RP
  {
    $2
  }
  | e1 = expression op = binop e2 = expression
  {
    Ast.Bin (e1, op, e2)
  }
  | op = unop e = expression %prec UMINUS
  {
    Ast.Un (op, e)
  }
  ;
