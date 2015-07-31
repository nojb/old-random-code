open Printf

type binop =
  | ADD | SUB | MUL | EQ | NEQ | LE
  | GE | LT |GT | AND | OR | POW | DIV
  | MOD

let string_of_binop op =
  match op with
  | ADD -> "+" | SUB -> "-"
  | MUL -> "*" | EQ -> "="
  | NEQ -> "<>" | LE -> "<="
  | GE -> ">=" | LT -> "<"
  | GT -> ">" | AND -> "AND"
  | OR -> "OR" | POW -> "^"
  | DIV -> "/" | MOD -> "MOD"

type ref =
  | VAR of var
  | SUBSCRIPT of var * exp list

and var =
  | STRINGVAR of string
  | FLOATVAR of string

and exp =
  | FLOAT of float
  | STRING of string
  | REF of ref
  | BIN of exp * binop * exp

let var_name v =
  match v with
  | STRINGVAR s -> s ^ "$"
  | FLOATVAR s -> s

type print_exp =
  | EXP of exp
  | TAB
  | NOSPACE

let string_of_fl x =
  let n = int_of_float x in
  if float_of_int n = x then string_of_int n
  else string_of_float x

let rec string_of_ref r =
  match r with
  | VAR v -> var_name v
  | SUBSCRIPT (v,el) ->
      sprintf "%s(%s)" (var_name v) (String.concat "," (List.map string_of_exp el))

and string_of_exp e =
  match e with
  | FLOAT x -> string_of_fl x
  | STRING s -> sprintf "\"%s\"" s
  | REF r -> string_of_ref r
  | BIN (e1, op, e2) ->
    sprintf "%s %s %s"
      (string_of_exp e1) (string_of_binop op) (string_of_exp e2)

let rec string_of_print_exp e =
  match e with
  | EXP e -> string_of_exp e
  | TAB -> ", "
  | NOSPACE -> ";"

type input_expr =
  | LINE of string
  | FLOATS of string list

type cmd =
  | READ of var list
  | DATA of exp list
  | STOP
  | REM of string
  | INPUT of string option * input_expr
  | PRINT of print_exp list
  | END
  | GOSUB of int
  | RETURN
  | LET of ref * exp
  | GOTO of int
  | CALL of var * exp list
  | IF of exp * cmd
  | FOR of string * exp * exp * exp option
  | NEXT of string option
  | DIM of (var * exp list) list

let rec string_of_cmd c =
  match c with
  | READ vl -> "READ " ^ (String.concat ", " (List.map var_name vl))
  | DATA el -> "DATA " ^ (String.concat ", " (List.map string_of_exp el))
  | STOP -> "STOP"
  | INPUT (x, LINE s) ->
    sprintf "INPUT %s%s$"
      (match x with None -> "" | Some u -> sprintf "\"%s\", " u) s
  | INPUT (x, FLOATS fl) ->
    sprintf "INPUT %s%s"
      (match x with None -> "" | Some u -> sprintf "\"%s\", " u)
      (String.concat ", " fl)
  | REM s -> "REM" ^ s
  | PRINT [] -> "PRINT"
  | PRINT el ->
    sprintf "PRINT " ^ (String.concat "" (List.map string_of_print_exp el))
  | END -> "END"
  | GOSUB i -> sprintf "GOSUB %d" i
  | RETURN -> "RETURN"
  | LET (r,e) -> sprintf "LET %s = %s" (string_of_ref r) (string_of_exp e)
  | GOTO i -> sprintf "GOTO %d" i
  | CALL (v, []) -> sprintf "CALL %s" (var_name v)
  | CALL (v, el) ->
      sprintf "CALL %s %s" (var_name v)
        (String.concat ", " (List.map string_of_exp el))
  | IF (e, c) -> sprintf "IF %s THEN %s" (string_of_exp e) (string_of_cmd c)
  | FOR (v, start, finish, None) ->
      sprintf "FOR %s = %s TO %s"
        v (string_of_exp start) (string_of_exp finish)
  | FOR (v, start, finish, Some step) ->
      sprintf "FOR %s = %s TO %s STEP %s"
        v (string_of_exp start) (string_of_exp finish) (string_of_exp step)
  | NEXT None -> "NEXT"
  | NEXT (Some v) -> "NEXT " ^ v
  | DIM al ->
      "DIM " ^ (String.concat ", "
        (List.map (fun (v,el) ->
          (var_name v) ^ "(" ^ (String.concat "," (List.map string_of_exp el)) ^ ")") al))

type directive =
  | RUN of int option
  | LIST
  | LOAD of string
  | SAVE of string
  | EXIT
  | STORE of int * cmd
  | DEL of int
  | NIL
