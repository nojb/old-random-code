module VariableKind :
  sig
    type t =
      | Var
      | Param
      | VarParam
  end

type variable_info =
  {
    variable_name : string;
    variable_kind : VariableKind.t;
    variable_level : int;
    mutable variable_is_set : bool;
  }

type symbol_info =
  | Variable of variable_info
  | Constant of constant_info
  | Procedure of procedure_info

and constant_info =
  {
    constant_name : string;
    constant_value : int;
  }

and free_variable_info =
  | Immediate of variable_info
  | Indirect of procedure_info

and procedure_info =
  {
    procedure_name : string;
    procedure_params : variable_info Queue.t;
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
  | Var of variable_info
  | Addr of variable_info
  | Const of constant_info

and condition =
  | Odd of expression
  | Rel of expression * Ast.relop * expression

and statement =
  | Assign of variable_info * expression
  | Call of call_info
  | Begin of statement list
  | If of condition * statement
  | While of condition * statement

val program : Ast.block_info -> block_info
