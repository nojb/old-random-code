type variable_info =
  {
    variable_name : string;
    variable_level : int;
  }

type parameter_info =
  {
    parameter_name : string;
    parameter_level : int;
    parameter_is_var : bool;
  }

type symbol_info =
  | Variable of variable_info
  | Constant of constant_info
  | Procedure of procedure_info
  | Parameter of parameter_info

and constant_info =
  {
    constant_name : string;
    constant_value : int;
  }

and free_variable_info =
  | FreeVariable of variable_info
  | FreeParameter of parameter_info
  | FreeCall of procedure_info

and procedure_info =
  {
    procedure_name : string;
    procedure_level : int;
    procedure_parameters : parameter_info Queue.t;
    mutable procedure_block : block_info option;
    procedure_free_variables : free_variable_info Queue.t;
    procedure_calls : call_info Queue.t;
  }

and block_info =
  {
    block_variables : variable_info list;
    block_procedures : procedure_info list;
    block_body : statement;
  }

and call_info =
  {
    call_procedure : procedure_info;
    call_args : expression Queue.t;
  }

and expression =
  | Int of int
  | Bin of expression * Ast.binop * expression
  | Un of Ast.unop * expression
  | Ref of reference
  | Addr of reference
  | Const of constant_info

and condition =
  | Odd of expression
  | Rel of expression * Ast.relop * expression

and statement =
  | Assign of reference * expression
  | Call of call_info
  | Begin of statement list
  | If of condition * statement
  | While of condition * statement

and reference =
  | Var of variable_info
  | Param of parameter_info

val program : Ast.block_info -> block_info
