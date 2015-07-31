open Semant

let some x =
  match x with
  | None -> failwith "[lift] internal error"
  | Some x -> x

let rec lift proc =
  let lifted =
    List.flatten (List.map lift
      (some proc.procedure_block).block_procedures)
  in
  let variables_to_be_lifted = Queue.create () in
  let visited_procedures = ref [] in
  let rec list_free_variables proc =
    visited_procedures := proc :: !visited_procedures;
    Queue.iter (fun v ->
      match v with
      | Immediate var ->
        begin
          (* we only add each free var once *)
          try Queue.iter (fun x -> if var == x then raise Exit)
            variables_to_be_lifted;
            Queue.push var variables_to_be_lifted
          with Exit -> ()
        end
      | Indirect proc ->
        begin
          if not (List.memq proc !visited_procedures) then
            list_free_variables proc
        end)
      proc.procedure_free_variables
  in
  let lift_free_var var =
    Queue.push { var with variable_kind = VariableKind.VarParam } proc.procedure_params;
    Queue.iter (fun call ->
      Queue.push (Addr var) call.call_args)
      proc.procedure_calls
  in
    list_free_variables proc;
    Queue.iter lift_free_var variables_to_be_lifted;
    proc::lifted

let program blk =
  {
    blk with block_procedures =
      List.flatten (List.map lift blk.block_procedures)
  }
