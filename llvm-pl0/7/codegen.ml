open Llvm
open Semant

let map_queue_to_array f q =
  if Queue.length q = 0 then [||]
  else let a = Array.make (Queue.length q) (Queue.top q) in
  let idx = ref 0 in
    Queue.iter (fun x -> a.(!idx) <- x; incr idx) q;
    Array.map f a

let some x =
  match x with
  | None -> failwith "[codegen] internal error @ some"
  | Some x -> x

let rec expression c m b env expr =
  match expr with
  | Int n -> const_int (i32_type c) n
  | Ref refn -> reference_load c m b env refn
  | Addr refn -> reference_address_load c m b env refn
  | Const const ->
      const_int (i32_type c) const.constant_value
  | Bin (e1, Ast.Mul, e2) ->
      let v1 = expression c m b env e1 in
      let v2 = expression c m b env e2 in
        build_mul v1 v2 "multmp" b
  | Bin (e1, Ast.Sub, e2) ->
      let v1 = expression c m b env e1 in
      let v2 = expression c m b env e2 in
        build_sub v1 v2 "subtmp" b
  | Bin (e1, Ast.Gt, e2) ->
      let v1 = expression c m b env e1 in
      let v2 = expression c m b env e2 in
        build_icmp Icmp.Sgt v1 v2 "cmptmp" b
  | _ -> failwith "[codegen] not implemented"

and reference_load c m b env refn =
  match refn with
  | Var var ->
      build_load (Env.find env var.variable_name) var.variable_name b
  | Param param ->
      build_load (Env.find env param.parameter_name) param.parameter_name b

and reference_address_load c m b env refn =
  match refn with
  | Var var ->
      Env.find env var.variable_name
  | Param param ->
      Env.find env param.parameter_name

and reference_store c m b env refn e =
  match refn with
  | Var var ->
      ignore (build_store (expression c m b env e)
        (Env.find env var.variable_name) b);
  | Param param ->
      ignore (build_store (expression c m b env e)
        (Env.find env param.parameter_name) b)

let rec statement c m b env stmt =
  match stmt with
  | Assign (refn, e) -> reference_store c m b env refn e
  | Call call ->
      ignore (build_call
        (Env.find env call.call_procedure.procedure_name)
        (map_queue_to_array (expression c m b env) call.call_args)
        call.call_procedure.procedure_name b)
  | Begin stmts -> List.iter (statement c m b env) stmts
  | While (cond, body) ->
    begin
      let parent = block_parent (insertion_block b) in
      let afterbb = append_block c "after" parent in
      let bodybb = append_block c "body" parent in
      let condbb = append_block c "test" parent in
      begin
        ignore (build_br condbb b);
        position_at_end condbb b;
        let cv = expression c m b env cond in
        begin

          (* FIXME cv is not of type i1 necessarily right now
           * because we are not doing type checking *)

          ignore (build_cond_br cv bodybb afterbb b);
          position_at_end bodybb b;
          List.iter (statement c m b env) body;
          ignore (build_br condbb b);
          position_at_end afterbb b
        end
      end
    end
  | _ -> failwith "[codegen] not implemented"

let base_env () =
  Env.create ()

let alloc_variable c b env var =
  Env.add env var.variable_name
    (build_alloca (i32_type c) var.variable_name b)

let declare_procedure m c env proc =
  let parm_type x =
    if x.parameter_is_var then
    begin
      pointer_type (i32_type c)
    end
    else
    begin
      i32_type c
    end
  in
  let funtype = function_type (i32_type c)
    (map_queue_to_array parm_type proc.procedure_parameters)
  in
  let funval = define_function proc.procedure_name funtype m
  in
    Env.add env proc.procedure_name funval

let define_procedure m c env proc =
  let n = proc.procedure_name in
  let funval = Env.find env n in
  let b = builder_at_end c (entry_block funval) in
  let env = Env.scope env in
    ignore (Queue.fold (fun idx parm ->

      (* Aesthetic. Give the right names to formal parameters
       * of generated functions *)

      set_value_name parm.parameter_name (param funval idx);

      (* If the formal parameter is a VAR parameter, then
       * we will be passing a pointer the value so we save
       * this pointer *)

      if parm.parameter_is_var then
      begin
        Env.add env parm.parameter_name (param funval idx);
        idx + 1
      end

      (* Otherwise, if it is a value parameter, then, since
       * it might be modified, as in
       * 
       * PROCEDURE f (a : INT);
       *   a := 12;
       *
       * we essentially allocate a mutable local variable with
       * and copy the argument value into that and save the pointer
       * to the local variable in the parameter env entry. In
       * some circumstances (but not all), this will be optimized
       * away by the mem2reg optimization if the parameter is not
       * in fact assigned to in the body of the procedure *)

      else
      begin
        let a = build_alloca (i32_type c) parm.parameter_name b in
        begin
          Env.add env parm.parameter_name a;
          ignore (build_store (param funval idx) a b);
          idx + 1
        end
      end)
      0 proc.procedure_parameters);

    (* Next, we generate code to allocate all the local variables *)

    List.iter (alloc_variable c b env) (some proc.procedure_block).block_variables;

    (* And we generate the code for the body of the block *)

    List.iter (statement c m b env) (procedure_body proc);

    (* TODO Is this always right? *)

    ignore (build_ret (const_int (i32_type c) 0) b)

let emit_debug_code c m b env prg =
  let printfunval = declare_function "_print"
    (function_type (i32_type c) [|i32_type c|]) m in
  let emit_var_dump var =
    ignore (build_call printfunval
      [|build_load (Env.find env var.variable_name) var.variable_name b|] "" b)
  in List.iter emit_var_dump prg.block_variables

let oberon_module om =
  let prg = om.module_block in
  let c = global_context () in
  let m = create_module c om.module_name in
  let b = builder c in
  let mainfuntype = function_type (void_type c) [||] in
  let mainfun = define_function "main" mainfuntype m in
  let env = base_env () in
    position_at_end (entry_block mainfun) b;
    List.iter (alloc_variable c b env) prg.block_variables;
    List.iter (declare_procedure m c env) prg.block_procedures;
    List.iter (define_procedure m c env) prg.block_procedures;
    List.iter (statement c m b env) prg.block_body;
    emit_debug_code c m b env prg;
    ignore (build_ret_void b);
    m
