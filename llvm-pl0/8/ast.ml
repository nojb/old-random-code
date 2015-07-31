type binop =
  | Add | Sub | Mul | Div
  | Eq | Neq | Lt | Le | Gt | Ge
  | Mod | And | Or

type unop =
  | Plus | Minus | Not

type type_info =
  | TypeName of string

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
    procedure_parameters : (bool * string * type_info) list;
    procedure_block : block_info;
  }

and block_info =
  {
    block_constants : (string * int) list;
    block_variables : (string * type_info) list;
    block_procedures : procedure_info list;
    block_body : statement list;
  }

type module_info =
  {
    module_name : string;
    module_block : block_info;
  }
