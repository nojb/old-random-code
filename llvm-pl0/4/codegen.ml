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
  | Var var ->
      if var.variable_kind = VariableKind.VarParam ||
        var.variable_kind = VariableKind.Var ||
        var.variable_is_set then
        build_load (Env.find env var.variable_name)
          var.variable_name b
      else
        Env.find env var.variable_name
  | Addr var ->
      if var.variable_kind = VariableKind.VarParam then
        Env.find env var.variable_name
      else if var.variable_kind = VariableKind.Var then
        Env.find env var.variable_name
      else failwith "[codegen] not implemented"
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
  | _ -> failwith "[codegen] not implemented"

let condition c m b env cond =
  match cond with
  | Rel (e1, Ast.Gt, e2) ->
      let v1 = expression c m b env e1 in
      let v2 = expression c m b env e2 in
        build_icmp Icmp.Sgt v1 v2 "cmptmp" b
  | _ -> failwith "[codegen] not implemented"

let rec statement c m b env stmt =
  match stmt with
  | Assign (var, e) ->
      ignore (build_store (expression c m b env e)
        (Env.find env var.variable_name) b);
  | Call call ->
      ignore (build_call
        (Env.find env call.call_procedure.procedure_name)
        (map_queue_to_array (expression c m b env) call.call_args)
        call.call_procedure.procedure_name b)
  | Begin stmts ->
      List.iter (statement c m b env) stmts
  | While (cond, body) ->
    begin
      let parent = block_parent (insertion_block b) in
      let afterbb = append_block c "after" parent in
      let bodybb = append_block c "body" parent in
      let condbb = append_block c "test" parent in
      begin
        ignore (build_br condbb b);
        position_at_end condbb b;
        let cv = condition c m b env cond in
        begin
          ignore (build_cond_br cv bodybb afterbb b);
          position_at_end bodybb b;
          statement c m b env body;
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
    match x.variable_kind with
    | VariableKind.VarParam -> pointer_type (i32_type c)
    | _ -> i32_type c
  in
  let funtype = function_type (i32_type c)
    (map_queue_to_array parm_type proc.procedure_params)
  in
  let funval = define_function proc.procedure_name funtype m
  in
    Env.add env proc.procedure_name funval

let define_procedure m c env proc =
  let n = proc.procedure_name in
  let funval = Env.find env n in
  let b = builder_at_end c (entry_block funval) in
  let env = Env.scope env in
    List.iter (alloc_variable c b env) (some proc.procedure_block).block_variables;
    ignore (Queue.fold (fun idx parm ->
      set_value_name parm.variable_name (param funval idx);
      if parm.variable_is_set && parm.variable_kind = VariableKind.Param then
        let a = build_alloca (i32_type c) parm.variable_name b in
        begin
          Env.add env parm.variable_name a;
          ignore (build_store (param funval idx) a b);
          idx + 1
        end
      else
        begin
          Env.add env parm.variable_name (param funval idx);
          idx + 1
        end)
      0 proc.procedure_params);
    statement c m b env (some proc.procedure_block).block_body;
    ignore (build_ret (const_int (i32_type c) 0) b)

let emit_debug_code c m b env prg =
  let printfunval = declare_function "_print"
    (function_type (i32_type c) [|i32_type c|]) m in
  let emit_var_dump var =
    ignore (build_call printfunval
      [|build_load (Env.find env var.variable_name) var.variable_name b|] "" b)
  in List.iter emit_var_dump prg.block_variables

let program prg =
  let c = global_context () in
  let m = create_module c "" in
  let b = builder c in
  let mainfuntype = function_type (void_type c) [||] in
  let mainfun = define_function "main" mainfuntype m in
  let env = base_env () in
    position_at_end (entry_block mainfun) b;
    List.iter (alloc_variable c b env) prg.block_variables;
    List.iter (declare_procedure m c env) prg.block_procedures;
    List.iter (define_procedure m c env) prg.block_procedures;
    statement c m b env prg.block_body;
    emit_debug_code c m b env prg;
    ignore (build_ret_void b);
    m
