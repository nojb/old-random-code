let map_queue_to_list f q =
  let lst = ref [] in
    Queue.iter (fun x -> lst := x :: ! lst) q;
    List.rev_map f !lst

let queue_to_list q = map_queue_to_list (fun x -> x) q

type expression =
  | Int of int
  | Bin of expression * Ast.binop * expression
  | Un of Ast.unop * expression
  | Var of string

type condition =
  | Odd of expression
  | Rel of expression * Ast.relop * expression

type statement =
  | Assign of string * expression
  | Call of string * expression list
  | Begin of statement list
  | If of condition * statement
  | While of condition * statement

type procedure_info =
  {
    procedure_name : string;
    procedure_params : string list;
    procedure_variables : string list;
    procedure_body : statement;
  }

type program =
  {
    program_variables : string list;
    program_procedures : procedure_info list;
    program_body : statement;
  }

let expression expr =
  match expr with
  | Semant.Int n -> Int n
  | Semant.Var var -> Var var.Semant.variable_name
  | _ -> failwith "[lift] not implemented"

let rec statement stmt =
  match stmt with
  | Semant.Assign (v, e) ->
      Assign (v.Semant.variable_name, expression e)
  | Semant.Call call ->
      Call (call.Semant.call_procedure.Semant.procedure_name,
        map_queue_to_list expression call.Semant.call_args)
  | Semant.Begin stmts ->
      Begin (List.map statement stmts)
  | _ -> failwith "[lift] not implemented"

let some x =
  match x with
  | None -> failwith "[lift] internal error"
  | Some x -> x

let procedure proc =
  {
    procedure_name =
      proc.Semant.procedure_name;
    procedure_params =
      map_queue_to_list (fun var -> var.Semant.variable_name)
        proc.Semant.procedure_params;
    procedure_variables =
      List.map (fun var -> var.Semant.variable_name)
        (some proc.Semant.procedure_block).Semant.block_variables;
    procedure_body =
      statement (some proc.Semant.procedure_block).Semant.block_body;

    (* we ignore proc.Semant.procedure_block.Semant.block_procedures
     * because they have already been lifted to top level *)
  }

let rec lift proc =
  print_endline ("[lift] " ^ proc.Semant.procedure_name);
  let lifted =
    List.flatten (List.map lift
      (some proc.Semant.procedure_block).Semant.block_procedures)
  in
  let variables_to_be_lifted = Queue.create () in
  let visited_procedures = ref [] in
  let rec list_free_variables proc =
    visited_procedures := proc :: !visited_procedures;
    Queue.iter (fun v ->
      match v with
      | Semant.Immediate var ->
        begin
          (* we only add each free var once *)
          try Queue.iter (fun x -> if var == x then raise Exit)
            variables_to_be_lifted;
            Queue.push var variables_to_be_lifted
          with Exit -> ()
        end
      | Semant.Indirect proc ->
        begin
          if not (List.memq proc !visited_procedures) then
            list_free_variables proc
        end)
      proc.Semant.procedure_free_variables
  in
  let lift_free_var var =
    Queue.push var proc.Semant.procedure_params;
    Queue.iter (fun call ->
      print_endline ("adding " ^ var.Semant.variable_name ^ " to a call to "
        ^ call.Semant.call_procedure.Semant.procedure_name);
      Queue.push (Semant.Var var) call.Semant.call_args)
      proc.Semant.procedure_calls
  in
    list_free_variables proc;
    print_endline ("variables to be lifted for " ^ proc.Semant.procedure_name);
    Queue.iter
      (fun var -> print_endline var.Semant.variable_name)
      variables_to_be_lifted;
    Queue.iter lift_free_var variables_to_be_lifted;
    print_endline ("params for " ^ proc.Semant.procedure_name);
    Queue.iter (fun var ->
      print_endline var.Semant.variable_name)
      proc.Semant.procedure_params;
    proc::lifted

let program blk =
  {
    program_variables =
      List.map
        (fun var -> var.Semant.variable_name)
        blk.Semant.block_variables;
    program_procedures =
      List.map procedure (List.flatten (List.map lift
        blk.Semant.block_procedures));
    program_body =
      statement blk.Semant.block_body;
  }
