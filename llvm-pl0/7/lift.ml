open Semant

let some x =
  match x with
  | None -> failwith "[lift] internal error"
  | Some x -> x

type liftkind =
  | LiftVariable of variable_info
  | LiftParameter of parameter_info

(* Here we use the list of free variables and procedure call
 * lists collected in the [Semant] phase to construct a final
 * list of free variables (and parameters). This boils down
 * to:
 *   1) Make sure each such only appears once
 *   2) We add all the free variables of a function call
 *      (provided it is the first time we see the function)
 *      but we omit those functions which are defined in
 *      any of the enclosing functions. *)

let rec list_free_variables visited freevars lvl proc =
  let add_freevar var =
    match var with
    | FreeVariable var ->
      begin

        (* We only add this free variable (that is, a free
         * variable of [proc] if it is not defined inside
         * the syntactically enclosing procedure, whose
         * level is [lvl] *)

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

        (* We only add this free variable (that is, a free
         * variable of [proc] if it is not defined inside
         * the syntactically enclosing procedure, whose
         * level is [lvl] *)

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

        (* We add the free variables of the called
         * procedure [proc], but we keep a list of
         * those procedures we have already seen,
         * so as not to loop for recursive calls. *)

        if not (List.memq proc !visited) then
          list_free_variables visited freevars lvl proc

      end
  in
  visited := proc :: !visited;
  Queue.iter add_freevar proc.procedure_free_variables

let param_from_lift = function
  | LiftVariable var ->
      
      (* FIXME level will be wrong ? *)

      {
        parameter_name = var.variable_name;
        parameter_level = var.variable_level;
        parameter_is_var = true;
      }

  | LiftParameter param ->

      (* FIXME level will be wrong! -- to fix this, we should store the
       * level in the procedure_info, and then pass that to param_from_list *)

      { param with parameter_is_var = true }

let arg_from_lift = function
  | LiftVariable var -> Addr (Var var)
  | LiftParameter param ->
      if param.parameter_is_var then Ref (Param param)
      else Addr (Param param)

(* Here we do the actual lambda-lifting. We return a
 * list of all the procedures with extra parameters
 * inserted and also we modify all calls to these
 * procedures to include the required extra arguments *)

let rec lift proc =
  let lifted =
    List.flatten (List.map lift
      (some proc.procedure_block).block_procedures)
  in
  let variables_to_be_lifted = Queue.create () in
  let visited_procedures = ref [] in
  let lift_free_var var =

    (* For each variable that we must lift, we add a formal
     * parameter to the procedure we are lifting [proc] and ... *)

    Queue.push (param_from_lift var) proc.procedure_parameters;

    (* ... also we add a corresponding argument to every call
     * to [proc] to pass the (address of) the variable *)

    Queue.iter (fun call ->
      Queue.push (arg_from_lift var) call.call_args)
      proc.procedure_calls

  in
    list_free_variables visited_procedures variables_to_be_lifted
      proc.procedure_level proc;
    Queue.iter lift_free_var variables_to_be_lifted;
    proc :: lifted

let oberon_module m =
  {
    m with module_block =
      {
        m.module_block with block_procedures =
          List.flatten (List.map lift m.module_block.block_procedures)
      }
  }
