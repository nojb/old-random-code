type binop =
  | Add | Sub | Mul | Div
  | Eq | Neq | Lt | Le | Gt | Ge

type unop =
  | Plus | Minus

type statement =
  | Assign of string * expression
  | Call of string * expression list
  | If of expression * statement list * (expression * statement list) list *
      statement list
  | While of expression * statement list

and expression =
  | Int of int
  | Bin of expression * binop * expression
  | Un of unop * expression
  | Var of string

and procedure_info =
  {
    procedure_name : string;
    procedure_parameters : (bool * string) list;
    procedure_block : block_info;
  }

and block_info =
  {
    block_constants : (string * int) list;
    block_variables : string list;
    block_procedures : procedure_info list;
    block_body : statement list;
  }

type module_info =
  {
    module_name : string;
    module_block : block_info;
  }
