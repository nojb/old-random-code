%token DOT EOF CONST COMMA SEMI VAR EQ
%token PROCEDURE IF THEN WHILE DO
%token BEGIN END LP RP LT LE GT GE
%token PLUS MINUS STAR SLASH NEQ COLONEQ
%token MODULE ELSIF ELSE

%token <int> INT
%token <string> IDENT

%nonassoc GT EQ GE LT LE NEQ
%left PLUS MINUS
%left STAR SLASH
%left UMINUS

%type <Ast.module_info> oberon_module
%start oberon_module

%%

oberon_module:
  MODULE IDENT SEMI block END IDENT DOT EOF
  {
    {
      Ast.module_name = $2;
      Ast.module_block = $4;
    }
  }
  ;

block:
  consts = constant_part vars = variable_part procs = list(procedure_part)
    body = loption(preceded(BEGIN, separated_nonempty_list(SEMI,statement)))
  {
    {
      Ast.block_constants = consts;
      Ast.block_variables = vars;
      Ast.block_procedures = procs;
      Ast.block_body = body;
    }
  }
  ;

constant_part:
  {
    []
  }
  | CONST consts = list(terminated(separated_pair(IDENT,EQ,INT),SEMI))
  {
    consts
  }
;

variable_part:
  {
    []
  }
  | VAR vars = list(terminated(variable_declarations, SEMI))
  {
    List.flatten vars
  }
  ;

variable_declarations:
  vars = separated_nonempty_list(COMMA,IDENT)
  {
    vars
  }
  ;

procedure_part:
  PROCEDURE name = IDENT params =
    loption(delimited(LP,separated_list(SEMI,formal_parameters_section),RP))
    SEMI body = block END IDENT SEMI
  {
    {
      Ast.procedure_name = name;
      Ast.procedure_parameters = List.flatten params;
      Ast.procedure_block = body;
    }
  }
  ;

formal_parameters_section:
  isvar = boption(VAR) params = separated_nonempty_list(COMMA,IDENT)
  {
    List.map (fun param -> (isvar, param)) params
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
  | IF cond = expression THEN then_part = separated_nonempty_list(SEMI,statement)
      elsifs_part = list(preceded(ELSIF,separated_pair(expression,THEN,separated_nonempty_list(SEMI,statement))))
      else_part = loption(preceded(ELSE,separated_nonempty_list(SEMI,statement))) END
  {
    Ast.If (cond, then_part, elsifs_part, else_part) 
  }
  | WHILE cond = expression DO body = separated_nonempty_list(SEMI,statement) END
  {
    Ast.While (cond, body)
  }
  ;

%inline binop:
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
  | PLUS
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
