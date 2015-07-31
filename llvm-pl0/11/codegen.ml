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

let build_binary_inst = function
  | Ast.Mul -> build_mul
  | Ast.Sub -> build_sub
  | Ast.Add -> build_add
  | Ast.Div -> build_sdiv
  | Ast.Gt -> build_icmp Icmp.Sgt
  | Ast.Ge -> build_icmp Icmp.Sge
  | Ast.Lt -> build_icmp Icmp.Slt
  | Ast.Le -> build_icmp Icmp.Sle
  | Ast.Mod -> build_srem
  | Ast.And -> build_and
  | Ast.Or -> build_or
  | _ -> failwith "[codegen] not implemented"

let build_unary_inst = function
  | Ast.Not -> build_not
  | _ -> failwith "[codegen] not implemented"

let rec expression c m b env expr =
  match expr.expression_desc with
  | Bool b -> const_int (i1_type c) (if b then 1 else 0)
  | Int n -> const_int (i32_type c) n
  | Ref refn -> reference_load c m b env refn
  | Addr refn -> reference_address_load c m b env refn
  | Const const ->
      const_int (i32_type c) const.constant_value
  | Bin (e1, op, e2) ->
      let v1 = expression c m b env e1 in
      let v2 = expression c m b env e2 in
        (build_binary_inst op) v1 v2 "tmp" b
  | Un (op, e) ->
      let v = expression c m b env e in
        (build_unary_inst op) v "tmp" b

and reference_load c m b env refn =
  match refn.reference_desc with
  | Var var ->
      build_load (Env.find env var.variable_name) var.variable_name b
  | Param param ->
      build_load (Env.find env param.parameter_name) param.parameter_name b
  | Array (base, idx) ->
    begin
      let baseval = reference_address_load c m b env base in
      let idx = expression c m b env idx in
      let gep = build_gep baseval
        [|const_int (i32_type c) 0; idx|] "gep" b
      in
        build_load gep "elt" b
    end
  | Field (base, field) ->
    begin
      let baseval = reference_address_load c m b env base in
      let gep = build_gep baseval
        [|const_int (i32_type c) 0;
        const_int (i32_type c) field.Types.field_index|] "gep" b
      in
        build_load gep "elt" b
    end

and reference_address_load c m b env refn =
  match refn.reference_desc with
  | Var var ->
      Env.find env var.variable_name
  | Param param ->
      Env.find env param.parameter_name
  | Array (base, idx) ->
    begin
      let baseval = reference_address_load c m b env base in
      let idx = expression c m b env idx in
        build_gep baseval [|const_int (i32_type c) 0; idx|] "tmp" b
    end
  | Field (base, field) ->
    begin
      let baseval = reference_address_load c m b env base in
        build_gep baseval
          [|const_int (i32_type c) 0;
          const_int (i32_type c) field.Types.field_index|] "elt" b
    end

and reference_store c m b env refn e =
  match refn.reference_desc with
  | Var var ->

      (* FIXME If var is a record then we should
       * copy all the fields, if it is an array, then
       * we should copy all the elements ... *)

      ignore (build_store (expression c m b env e)
        (Env.find env var.variable_name) b);

  | Param param ->

      (* FIXME Same as above *)

      ignore (build_store (expression c m b env e)
        (Env.find env param.parameter_name) b)

  | Array (base, idx) ->
    begin
      let baseval = reference_address_load c m b env base in
      let idx = expression c m b env idx in
      let gep = build_gep baseval
        [|const_int (i32_type c) 0; idx|] "tmp" b
      in
        ignore (build_store (expression c m b env e) gep b)
    end
  | Field (base, field) ->
    begin
      let baseval = reference_address_load c m b env base in
      let gep = build_gep baseval
        [|const_int (i32_type c) 0;
        const_int (i32_type c) field.Types.field_index|] "elt" b
      in
        ignore (build_store (expression c m b env e) gep b)
    end

let rec statement c m b env stmt =
  match stmt with
  | Assign (refn, e) -> reference_store c m b env refn e
  | Call call ->
      ignore (build_call
        (Env.find env call.call_procedure.procedure_name)
        (map_queue_to_array (expression c m b env) call.call_args)
        call.call_procedure.procedure_name b)
  | CallPrimitive (prim, args) ->
    begin
      let primval = declare_function prim.primitive_implementation_name
        (function_type (i32_type c) (Array.of_list
          (List.map (fun arg -> Types.to_lltype c arg.expression_type) args)))
          m
      in
        ignore (build_call primval (Array.of_list
          (List.map (expression c m b env) args)) prim.primitive_name b)
    end
  | If (elsifs, els) ->
    begin
      let current = insertion_block b in
      let parent = block_parent current in
      let elsebb = append_block c "else" parent in
      let mergebb = append_block c "merge" parent in

        (* This gets called for every one of the
         * branches of the IF (including the first
         * one, but not the last one -- that is, the
         * ELSE part) *)

        let handle_one_case (cond, body) elsebb =
          let testbb = append_block c "test" parent in
          let thenbb = append_block c "then" parent in

            (* Emit code for the condition *)

            position_at_end testbb b;
            let condval = expression c m b env cond in
              ignore (build_cond_br condval thenbb elsebb b);

              (* Emit code for the consequence of the
               * condition (that is, the THEN part) *)

              position_at_end thenbb b;
              List.iter (statement c m b env) body;

              (* Finally, branch to the merge node *)

              ignore (build_br mergebb b);

              testbb

        in let testbb =
          List.fold_right handle_one_case elsifs elsebb
        in

          (* Position the builder at the end of the
           * block that existed before we wrote out
           * all the above branching code and write
           * out an unconditional branch to the first
           * condition of the IF *)

          position_at_end current b;
          ignore (build_br testbb b);

          (* Emit code for the ELSE part of the
           * IF. We just emit code for the statements
           * and then branch to the merge instruction *)

          position_at_end elsebb b;
          List.iter (statement c m b env) els;
          ignore (build_br mergebb b);

          (* Finally, we put ourselves at the very end of
           * the merge basic block *)

          position_at_end mergebb b
    end
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

let base_env () =
  Env.create ()

let alloc_variable c b env var =
  Env.add env var.variable_name
    (build_alloca (Types.to_lltype c var.variable_type) var.variable_name b)

let declare_procedure m c env proc =
  let parm_type param =
    if param.parameter_is_var then
      pointer_type (Types.to_lltype c param.parameter_type)
    else
      Types.to_lltype c param.parameter_type
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
        let a = build_alloca (Types.to_lltype c parm.parameter_type) parm.parameter_name b in
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

let alloc_global c b m env var =
  let gl = define_global var.variable_name
    (undef (Types.to_lltype c var.variable_type)) m
  in
    Env.add env var.variable_name gl

let oberon_module om =
  let prg = om.module_block in
  let c = global_context () in
  let m = create_module c om.module_name in
  let b = builder c in
  let mainfuntype = function_type (void_type c) [||] in
  let mainfun = define_function "main" mainfuntype m in
  let env = base_env () in
    position_at_end (entry_block mainfun) b;
    List.iter (alloc_global c b m env) prg.block_variables;
    List.iter (declare_procedure m c env) prg.block_procedures;
    List.iter (define_procedure m c env) prg.block_procedures;
    List.iter (statement c m b env) prg.block_body;
    ignore (build_ret_void b);
    m
