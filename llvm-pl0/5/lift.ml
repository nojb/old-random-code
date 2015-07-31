open Semant

let some x =
  match x with
  | None -> failwith "[lift] internal error"
  | Some x -> x

type liftkind =
  | LiftVariable of variable_info
  | LiftParameter of parameter_info

let rec list_free_variables visited freevars lvl proc =
  let add_freevar var =
    match var with
    | FreeVariable var ->
      begin
        if lvl >= var.variable_level then
        begin
        try Queue.iter (fun x ->
          match x with
          | LiftVariable var2 -> if var == var2 then raise Exit
          | _ -> ())
          freevars;
          Queue.push (LiftVariable var) freevars
        with Exit -> ()
        end
      end
    | FreeParameter param ->
      begin
        if lvl >= param.parameter_level then
        begin
        try Queue.iter (fun x ->
          match x with
          | LiftParameter param2 -> if param == param2 then raise Exit
          | _ -> ())
          freevars;
          Queue.push (LiftParameter param) freevars
        with Exit -> ()
        end
      end
    | FreeCall proc ->
      begin
        if not (List.memq proc !visited) then
          list_free_variables visited freevars lvl proc
      end
  in
  visited := proc :: !visited;
  Queue.iter add_freevar proc.procedure_free_variables

let param_from_lift = function
  | LiftVariable var ->
      {
        parameter_name = var.variable_name;
        parameter_level = var.variable_level; (* level will be wrong ? *)
        parameter_is_var = true;
      }
  | LiftParameter param ->
      { param with parameter_is_var = true }
      (* warning level will be wrong! -- to fix this, we should store the
       * level in the procedure_info, and then pass that to param_from_list *)

let arg_from_lift = function
  | LiftVariable var -> Addr (Var var)
  | LiftParameter param ->
      if param.parameter_is_var then Ref (Param param)
      else Addr (Param param)

let rec lift proc =
  let lifted =
    List.flatten (List.map lift
      (some proc.procedure_block).block_procedures)
  in
  let variables_to_be_lifted = Queue.create () in
  let visited_procedures = ref [] in
  let lift_free_var var =
    Queue.push (param_from_lift var) proc.procedure_parameters;
    Queue.iter (fun call ->
      Queue.push (arg_from_lift var) call.call_args)
      proc.procedure_calls
  in
    list_free_variables visited_procedures variables_to_be_lifted
      proc.procedure_level proc;
    Queue.iter lift_free_var variables_to_be_lifted;
    proc :: lifted

let program blk =
  {
    blk with block_procedures =
      List.flatten (List.map lift blk.block_procedures)
  }
