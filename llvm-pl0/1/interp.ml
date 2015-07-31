type symbol_info =
  | Constant of int
  | Variable of int
  | Procedure of Ast.procedure_info

module Tbl = Map.Make (
  struct
    type t = string
    let compare x y =
      compare (String.lowercase x) (String.lowercase y)
  end)

let dump_symbols tbl =
  let dump_symbol_info n = function
    | Constant x -> Printf.printf "CONST %s = %d\n" n x
    | Variable x -> Printf.printf "VAR %s = %d\n" n x
    | Procedure pi -> Printf.printf "PROC %s\n" pi.Ast.procedure_name
  in 
    Tbl.iter dump_symbol_info tbl

let relop_imp op =
  match op with
  | Ast.Eq -> (fun x y -> x = y)
  | Ast.Neq -> (fun x y -> x <> y)
  | Ast.Lt -> (fun x y -> x < y)
  | Ast.Le -> (fun x y -> x <= y)
  | Ast.Gt -> (fun x y -> x > y)
  | Ast.Ge -> (fun x y -> x >= y)

let binop_imp op =
  match op with
  | Ast.Add -> (fun x y -> x + y)
  | Ast.Mul -> (fun x y -> x * y)
  | Ast.Sub -> (fun x y -> x - y)
  | Ast.Div -> (fun x y -> x / y)

let unop_imp op =
  match op with
  | Ast.Plus -> (fun x -> x)
  | Ast.Minus -> (fun x -> -x)

let rec interp_statement env stmt =
  match stmt with
  | Ast.Assign (n, e) ->
    begin
      try match Tbl.find n env with
      | Variable _ ->
        begin
          let e = interp_expression env e in
          let env = Tbl.add n (Variable e) env in
            Printf.printf "  %s <- %d\n" n e;
            env
        end
      | _ -> failwith "not a variable"
      with Not_found -> failwith "variable not found"
    end
  | Ast.Call (n) ->
    begin
      Printf.printf ">>> %s\n" n;
      try match Tbl.find n env with
        | Procedure pi ->
          begin
            let env = interp_block env pi.Ast.procedure_block in
              Printf.printf "<<< %s\n" n;
              env
          end
        | _ -> failwith "not a procedure"
      with Not_found -> failwith "procedure not found"
    end
  | Ast.Begin (stmts) ->
      List.fold_left interp_statement env stmts
  | Ast.If (cond, stmt) ->
    begin
      if interp_condition env cond
      then interp_statement env stmt
      else env
    end
  | Ast.While (cond, stmt) ->
    begin
      let rec loop env =
        if interp_condition env cond then
          loop (interp_statement env stmt)
        else
          env
      in loop env
    end

and interp_condition env cond =
  match cond with
  | Ast.Odd e ->
      ((interp_expression env e) mod 2) <> 0
  | Ast.Rel (e1, op, e2) ->
      (relop_imp op) (interp_expression env e1) (interp_expression env e2)

and interp_expression env expr =
  match expr with
  | Ast.Int (n) -> n
  | Ast.Bin (e1, op, e2) ->
      (binop_imp op) (interp_expression env e1) (interp_expression env e2)
  | Ast.Un (op, e) ->
      (unop_imp op) (interp_expression env e)
  | Ast.Var (n) ->
    begin
      try match Tbl.find n env with
      | Variable x -> x
      | Constant x -> x
      | _ -> failwith "not a variable or constant"
      with Not_found -> failwith "variable not found"
    end

and interp_block env bi =
  let env = List.fold_left (fun env (n, x) -> Tbl.add n (Constant x) env) env
    bi.Ast.block_constants in
  let env = List.fold_left (fun env n -> Tbl.add n (Variable 0) env) env
    bi.Ast.block_variables in
  let env = List.fold_left (fun env pi -> Tbl.add pi.Ast.procedure_name
    (Procedure pi) env) env bi.Ast.block_procedures
  in
    interp_statement env bi.Ast.block_body

let interp_program bi =
  ignore (interp_block Tbl.empty bi)
