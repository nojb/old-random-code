type binop =
  | Add | Sub | Mul | Div
  | Eq | Neq | Lt | Le | Gt | Ge
  | Mod | And | Or

type unop =
  | Plus | Minus | Not

type statement =
  | Assign of reference * expression
  | Call of string * expression list
  | If of expression * statement list * (expression * statement list) list *
      statement list
  | While of expression * statement list

and expression =
  | Int of int
  | Bin of expression * binop * expression
  | Un of unop * expression
  | Ref of reference

and reference =
  | Var of string
  | Array of reference * expression
  | Field of reference * string

and type_info =
  | TypeName of string
  | TypeArray of expression * type_info
  | TypeRecord of (string * type_info) list

and procedure_info =
  {
    procedure_name : string;
    procedure_parameters : (bool * string * type_info) list;
    procedure_block : block_info;
  }

and block_info =
  {
    block_constants : (string * int) list;
    block_types : (string * type_info) list;
    block_variables : (string * type_info) list;
    block_procedures : procedure_info list;
    block_body : statement list;
  }

type module_info =
  {
    module_name : string;
    module_block : block_info;
  }
