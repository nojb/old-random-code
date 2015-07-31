%token DOT EOF CONST COMMA SEMI VAR EQ
%token PROCEDURE IF THEN WHILE DO
%token BEGIN END LP RP LT LE GT GE
%token PLUS MINUS STAR TILDE SHARP COLONEQ
%token MODULE ELSIF ELSE COLON DIV MOD
%token AMPERSAND OR ARRAY RECORD OF
%token RB LB TYPE TRUE FALSE

%token <int> INT
%token <string> IDENT

%left OR
%left AMPERSAND
%nonassoc GT EQ GE LT LE SHARP
%left PLUS MINUS
%left STAR DIV MOD
%left UMINUS

%type <Ast.module_info> oberon_module
%start oberon_module

%%

oberon_module:
  MODULE IDENT SEMI block END option(IDENT) DOT EOF
  (* we allow to skip the identifier after the END *)
  {
    {
      Ast.module_name = $2;
      Ast.module_block = $4;
    }
  }
  ;

block:
  consts = constant_part types = type_part vars = variable_part
    procs = list(procedure_part)
    body = loption(preceded(BEGIN, separated_nonempty_list(SEMI,statement)))
  {
    {
      Ast.block_constants = consts;
      Ast.block_types = types;
      Ast.block_variables = vars;
      Ast.block_procedures = procs;
      Ast.block_body = body;
    }
  }
  ;

type_part:
  (* nothing *)
  {
    []
  }
  | TYPE types = list(terminated(separated_pair(IDENT,EQ,type_specification),SEMI))
  {
    types
  }
  ;

type_specification:
  IDENT
  {
    Ast.TypeName $1
  }
  | ARRAY expression OF type_specification
  {
    Ast.TypeArray ($2, $4)
  }
  | RECORD fields = separated_nonempty_list(SEMI,field_list) END
  {
    Ast.TypeRecord (List.flatten fields)
  }
  ;

field_list:
  fields = separated_nonempty_list(COMMA,IDENT) COLON t = type_specification
  {
    List.map (fun field -> (field, t)) fields
  }
  ;

constant_part:
  (* nothing *)
  {
    []
  }
  | CONST consts = list(terminated(separated_pair(IDENT,EQ,INT),SEMI))
  {
    consts
  }
;

variable_part:
  (* nothing *)
  {
    []
  }
  | VAR vars = list(terminated(variable_declarations, SEMI))
  {
    List.flatten vars
  }
  ;

variable_declarations:
  vars = separated_nonempty_list(COMMA,IDENT) COLON t = type_specification
  {
    List.map (fun var -> (var, t)) vars
  }
  ;

procedure_part:
  PROCEDURE name = IDENT params =
    loption(delimited(LP,separated_list(SEMI,formal_parameters_section),RP))
    SEMI body = block END option(IDENT) SEMI
  (* we allow to skip the identifier after the final END *)
  {
    {
      Ast.procedure_name = name;
      Ast.procedure_parameters = List.flatten params;
      Ast.procedure_block = body;
    }
  }
  ;

formal_parameters_section:
  isvar = boption(VAR) params = separated_nonempty_list(COMMA,IDENT) COLON
    t = type_specification
  {
    List.map (fun param -> (isvar, param, t)) params
  }
  ;

statement:
  reference COLONEQ expression
  {
    Ast.Assign ($1, $3)
  }
  | name = IDENT args = loption(delimited(LP,separated_nonempty_list(COMMA,expression),RP))
  {
    Ast.Call (name, args)
    (*match name with
    | Ast.Var name -> Ast.Call (name, args)
    | _ -> $syntaxerror*)
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
  | SHARP
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
  | DIV
  {
    Ast.Div
  }
  | MOD
  {
    Ast.Mod
  }
  | AMPERSAND
  {
    Ast.And
  }
  | OR
  {
    Ast.Or
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
  | TILDE
  {
    Ast.Not
  }
  ;

expression:
  reference
  {
    Ast.Ref ($1)
  }
  | TRUE
  {
    Ast.Bool true
  }
  | FALSE
  {
    Ast.Bool false
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

reference:
  IDENT
  {
    Ast.Var ($1)
  }
  | reference LB expression RB
  {
    Ast.Array ($1, $3)
  }
  | reference DOT IDENT
  {
    Ast.Field ($1, $3)
  }
  ;
