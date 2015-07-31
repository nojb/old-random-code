type relop =
  | Eq | Neq | Lt | Le | Gt | Ge

type binop =
  | Add | Sub | Mul | Div

type unop =
  | Plus | Minus

type statement =
  | Assign of string * expression
  | Call of string * expression list
  | Begin of statement list
  | If of condition * statement
  | While of condition * statement

and condition =
  | Odd of expression
  | Rel of expression * relop * expression

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
    block_body : statement;
  }
