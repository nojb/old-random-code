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

let free_variable_name v =
  match v with
  | FreeVariable var -> "FreeVar " ^ var.variable_name
  | FreeParameter param -> "FreeParam " ^ param.parameter_name
  | FreeCall proc -> "FreeCall " ^ proc.procedure_name

let free_variable_level v =
  match v with
  | FreeVariable var -> var.variable_level
  | FreeParameter param -> param.parameter_level
  | FreeCall proc -> proc.procedure_level

let rec expression lvl env add_free_var expr =
  match expr with
  | Ast.Int n -> Int n
  | Ast.Bin (e1, op, e2) ->
      Bin (expression lvl env add_free_var e1, op, expression lvl env
        add_free_var e2)
  | Ast.Un (op, e) ->
      Un (op, expression lvl env add_free_var e)
  | Ast.Var n ->
    begin
      try match Env.find env n with
      | Variable var ->
        begin
          if var.variable_level < lvl then (* free variable *)
            add_free_var (FreeVariable var);
          Ref (Var var)
        end
      | Parameter param ->
        begin
          if param.parameter_level < lvl then
            add_free_var (FreeParameter param);
          Ref (Param param)
        end
      | Constant value -> Const value
      | Procedure _ ->
          failwith "[semant] can't use proc in expression"
      with Not_found ->
        failwith "[semant] variable not found"
    end

let condition lvl env add_free_var cond =
  match cond with
  | Ast.Rel (e1, op, e2) ->
      Rel (expression lvl env add_free_var e1,
        op, expression lvl env add_free_var e2)
  | _ -> failwith "[semant] not implemented"

let rec statement lvl env add_free_var stmt =
  match stmt with
  | Ast.Assign (n, e) ->
    begin
      try match Env.find env n with
      | Variable var ->
        begin
          if var.variable_level < lvl then
            add_free_var (FreeVariable var);
          Assign (Var var, expression lvl env add_free_var e)
        end
      | Parameter param ->
        begin
          if param.parameter_level < lvl then
            add_free_var (FreeParameter param);
          Assign (Param param, expression lvl env add_free_var e)
        end
      | Constant _ ->
          failwith "[semant] can't modify constant"
      | Procedure _ ->
          failwith "[semant] can't assign procedure"
      with Not_found ->
        failwith "[semant] variable not found"
    end
  | Ast.Call (n, args) ->
    begin
      try match Env.find env n with
      | Procedure proc ->
        begin
          let call =
            {
              call_procedure = proc;
              call_args = Queue.create ();
            }
          in
          let args = List.map (expression lvl env add_free_var) args in
            List.iter (fun x -> Queue.push x call.call_args) args;
            add_free_var (FreeCall proc);
            Queue.push call proc.procedure_calls;
            Call call
        end
      | Variable _ ->
          failwith "[semant] can't call variable"
      | Parameter _ ->
          failwith "[semant] can't call param"
      | Constant _ ->
          failwith "[semant] can't call constant"
      with Not_found ->
        failwith "[semant] procedure not found"
    end
  | Ast.Begin stmts ->
      Begin (List.map (statement lvl env add_free_var) stmts)
  | Ast.While (cond, body) ->
      While (condition lvl env add_free_var cond,
        statement lvl env add_free_var body)
  | _ -> failwith "[semant] not implemented"

let base_env () =
  Env.create ()

let constant n v =
  {
    constant_name = n;
    constant_value = v;
  }

let variable n lvl =
  {
    variable_name = n;
    variable_level = lvl;
  }

let rec block lvl env add_free_var blk =
  let lvl = lvl + 1 in
  let env = Env.scope env in
  let add_const (n, v) =
    Env.add env n (Constant (constant n v))
  in
  let add_var n =
    let var = variable n lvl in
      Env.add env n (Variable var);
      var
  in
  let add_proc_header proc =
    let n = proc.Ast.procedure_name in
    let prc = 
      {
        procedure_name = n;
        procedure_level = lvl;
        procedure_parameters = Queue.create ();
        procedure_block = None;
        procedure_free_variables = Queue.create ();
        procedure_calls = Queue.create ();
      }
    in
      List.iter (fun x ->
        Queue.push
          {
            parameter_name = x;
            parameter_level = lvl+1;
            parameter_is_var = false;
          }
          prc.procedure_parameters)
        proc.Ast.procedure_parameters;
      Env.add env n (Procedure prc);
      prc
  in
  let add_proc proc =
    let n = proc.Ast.procedure_name in
    try match Env.find env n with
    | Procedure prc ->
      begin
        let add_free_var v =
          print_endline ("add_free_var : " ^ (free_variable_name v) ^ " @ " ^
            (string_of_int (free_variable_level v)));
          Queue.push v prc.procedure_free_variables
        in
          Queue.iter (fun param ->
            Env.add env param.parameter_name (Parameter param))
            prc.procedure_parameters;
          print_endline ("block : about to process body of " ^
            prc.procedure_name);
          prc.procedure_block <-
            Some (block lvl env add_free_var proc.Ast.procedure_block);
          print_endline ("block : done processing " ^ prc.procedure_name)
      end
    | _ ->
        failwith "[semant] internal error"
    with Not_found ->
      failwith "[semant] internal error"
  in
    List.iter add_const blk.Ast.block_constants;
    let vars = List.map add_var blk.Ast.block_variables in
    let procs = List.map add_proc_header blk.Ast.block_procedures in
      List.iter add_proc blk.Ast.block_procedures;
      let stmt = statement lvl env add_free_var blk.Ast.block_body in
        {
          block_variables = vars;
          block_procedures = procs;
          block_body = stmt;
        }

let program blk =
  block (-1) (base_env ()) (fun _ -> ()) blk
