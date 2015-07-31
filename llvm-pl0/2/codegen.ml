open Llvm

let rec expression c m b env expr =
  match expr with
  | Lift.Int n -> const_int (i32_type c) n
  | Lift.Var n -> build_load (Env.find env n) n b
  | _ -> failwith "[codegen] not implemented"

let rec statement c m b env stmt =
  match stmt with
  | Lift.Assign (n, e) ->
    begin
      print_endline ("emitting assign for " ^ n);
      ignore (build_store (expression c m b env e) (Env.find env n) b);
      print_endline ("done emitting assign for " ^ n)
    end
  | Lift.Call (n, args) ->
      ignore (build_call
        (Env.find env n)
        (Array.of_list (List.map (expression c m b env) args))
        n b)
  | Lift.Begin stmts ->
      List.iter (statement c m b env) stmts
  | _ -> failwith "[codegen] not implemented"

let base_env () =
  Env.create ()

let alloc_variable c b env n =
  Env.add env n (build_alloca (i32_type c) n b)

let declare_procedure m c env proc =
  let funtype = function_type (i32_type c)
    (Array.make (List.length proc.Lift.procedure_params) (i32_type c))
  in
  let funval = define_function proc.Lift.procedure_name
    funtype m
  in
    Env.add env proc.Lift.procedure_name funval

let define_procedure m c env proc =
  let n = proc.Lift.procedure_name in
  let funval = Env.find env n in
  let b = builder_at_end c (entry_block funval) in
  print_endline ("[codegen] found proc " ^ n);
  let env = Env.scope env in
    List.iter (alloc_variable c b env) proc.Lift.procedure_variables;
    print_endline ("[codegen] done allocating variables for " ^ n);
    let rec loop idx lst =
      match lst with
      | [] -> ()
      | n::rest ->
        begin
          let a = build_alloca (i32_type c) n b in
            Env.add env n a;
            ignore (build_store (param funval idx) a b);
            loop (idx+1) rest
            (*Env.add env n (param funval idx); loop (idx+1) rest;*)
        end
    in loop 0 proc.Lift.procedure_params;
    print_endline "[codegen] done allocating params";
    statement c m b env proc.Lift.procedure_body

let program prg =
  let c = global_context () in
  let m = create_module c "" in
  let b = builder c in
  let mainfuntype = function_type (void_type c) [||] in
  let mainfun = define_function "main" mainfuntype m in
  let env = base_env () in
    position_at_end (entry_block mainfun) b;
    List.iter (alloc_variable c b env) prg.Lift.program_variables;
    List.iter (declare_procedure m c env) prg.Lift.program_procedures;
    print_endline "about to call define_procedure";
    List.iter (define_procedure m c env) prg.Lift.program_procedures;
    print_endline "done calling define_procedure";
    statement c m b env prg.Lift.program_body;
    m
