type variable_info =
  {
    variable_name : string;
    variable_level : int;
    variable_type : Types.type_info;
  }

type parameter_info =
  {
    parameter_name : string;
    parameter_level : int;
    parameter_is_var : bool;
    parameter_type : Types.type_info;
  }

type symbol_info =
  | Variable of variable_info
  | Constant of constant_info
  | Procedure of procedure_info
  | Parameter of parameter_info
  | Type of Types.type_info

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
    block_body : statement list;
  }

and call_info =
  {
    call_procedure : procedure_info;
    call_args : expression_with_type Queue.t;
  }

and expression =
  | Int of int
  | Bin of expression_with_type * Ast.binop * expression_with_type
  | Un of Ast.unop * expression_with_type
  | Ref of reference
  | Addr of reference
  | Const of constant_info

and statement =
  | Assign of reference * expression_with_type
  | Call of call_info
  | If of (expression_with_type * statement list) list * statement list
  | While of expression_with_type * statement list

and reference =
  | Var of variable_info
  | Param of parameter_info

and expression_with_type =
  {
    expression_desc : expression;
    expression_type : Types.type_info;
  }

type module_info =
  {
    module_name : string;
    module_block : block_info;
  }

val procedure_body : procedure_info -> statement list

val oberon_module : Ast.module_info -> module_info
