type variable_info =
  {
    variable_name : string;
    variable_level : int;
    variable_type : Types.type_info;
  }

type parameter_info =
  {
    parameter_name : string;
    parameter_level : int;
    parameter_is_var : bool;
    parameter_type : Types.type_info;
  }

type symbol_info =
  | Variable of variable_info
  | Constant of constant_info
  | Procedure of procedure_info
  | Parameter of parameter_info
  | Type of Types.type_info

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
    block_body : statement list;
  }

and call_info =
  {
    call_procedure : procedure_info;
    call_args : expression_with_type Queue.t;
  }

and expression =
  | Int of int
  | Bin of expression_with_type * Ast.binop * expression_with_type
  | Un of Ast.unop * expression_with_type
  | Ref of reference_with_type
  | Addr of reference_with_type
  | Const of constant_info

and statement =
  | Assign of reference_with_type * expression_with_type
  | Call of call_info
  | If of (expression_with_type * statement list) list * statement list
  | While of expression_with_type * statement list

and reference =
  | Var of variable_info
  | Param of parameter_info
  | Array of reference_with_type * expression_with_type

and expression_with_type =
  {
    expression_desc : expression;
    expression_type : Types.type_info;
  }

and reference_with_type =
  {
    reference_desc : reference;
    reference_type : Types.type_info;
  }

type module_info =
  {
    module_name : string;
    module_block : block_info;
  }

let procedure_body proc =
  match proc.procedure_block with
  | None -> failwith "procedure_body: procedure_block is None"
  | Some blk -> blk.block_body

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

let operand_type = function
  | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Mod
  | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge -> Types.integer
  | Ast.Eq | Ast.Neq -> Types.integer (* FIXME Can also be used with booleans *)
  | Ast.And | Ast.Or -> Types.boolean

let result_type = function
  | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Mod -> Types.integer
  | Ast.Eq | Ast.Neq | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge -> Types.boolean
  | Ast.And | Ast.Or -> Types.boolean

let unary_operand_type = function
  | Ast.Plus | Ast.Minus -> Types.integer
  | Ast.Not -> Types.boolean

let unary_result_type = function
  | Ast.Plus | Ast.Minus -> Types.integer
  | Ast.Not -> Types.boolean

let rec expression lvl env add_free_var expr =
  match expr with
  | Ast.Int n ->
      {
        expression_desc = Int n;
        expression_type = Types.integer;
      }
  | Ast.Bin (e1, op, e2) ->
      let e1 = expression lvl env add_free_var e1 in
      let e2 = expression lvl env add_free_var e2 in
        if not (Types.equal e1.expression_type e2.expression_type) then
          failwith "incompatible types in binary operation"
        else if not (Types.equal e1.expression_type (operand_type op)) then
          failwith "mistyped operands in binary operation"
        else
          {
            expression_desc = Bin (e1, op, e2);
            expression_type = result_type op;
          }
  | Ast.Un (op, e) ->
      let e = expression lvl env add_free_var e in
        if not (Types.equal e.expression_type (unary_operand_type op)) then
          failwith "mistyped operand in unary operation"
        else
          {
            expression_desc = Un (op, e);
            expression_type = unary_result_type op;
          }
  | Ast.Ref refn ->
      let refn = reference lvl env add_free_var refn in
        {
          expression_desc = Ref refn;
          expression_type = refn.reference_type;
        }

and reference lvl env add_free_var refn =
  match refn with
  | Ast.Var n ->
    begin
      try match Env.find env n with
      | Variable var ->
        begin
          
          (* free variable *)

          if var.variable_level < lvl then
            add_free_var (FreeVariable var);

          {
            reference_desc = Var var;
            reference_type = var.variable_type;
          }
        end
      | Parameter param ->
        begin

          (* free parameter *)

          if param.parameter_level < lvl then
            add_free_var (FreeParameter param);

          {
            reference_desc = Param param;
            reference_type = param.parameter_type;
          }
        end
      | Constant _ ->
          failwith "[semant] not implemented"
      | Procedure _ ->
          failwith "[semant] can't use proc in expression"
      | Type _ ->
          failwith "[semant] can't use type in expression"
      with Not_found ->
        failwith "[semant] variable not found"
    end
  | Ast.Array (refn, idx) ->
    begin
      let base = reference lvl env add_free_var refn in
      let idx = expression_with_type lvl env add_free_var idx Types.integer in
        if not (Types.is_array base.reference_type) then
          failwith "reference not of array type"
        else
          {
            reference_desc = Array (base, idx);
            reference_type = Types.array_element_type base.reference_type;
          }
    end

and expression_with_type lvl env add_free_var expr t =
  let expr = expression lvl env add_free_var expr in
    if not (Types.equal expr.expression_type t) then
      failwith "expression is not of correct type"
    else
      expr

let getref expr =
  match expr.expression_desc with
  | Ref (refn) ->
      {
        expression_desc = Addr refn;
        expression_type = expr.expression_type;
      }
  | _ -> failwith "[semant] getref : not a reference"

(* FIXME evaluate_expression should also evaluate
 * Boolean expressions, etc ? *)

let rec evaluate_expression env = function
  | Ast.Int n -> n
  | _ -> failwith "[semant] not implemented"

let rec statement lvl env add_free_var stmt =
  match stmt with
  | Ast.Assign (r, e) ->
      let r = reference lvl env add_free_var r in
      let e = expression lvl env add_free_var e in

        (* TODO Later on, we might want to check for
         * 'type-compatiblity' (whatever that is) *)

        if not (Types.equal r.reference_type e.expression_type) then
          failwith "type mismatch in assignment"
        else
          Assign (r, e)

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

            (* Check that we have the right arity *)

            if List.length args != Queue.length proc.procedure_parameters then
              failwith "[semant] wrong number of arguments";

            (* Next, we add the analyzed expressions to [call]. But
             * if any of the parameters is of variable type, we must
             * pass the address of the resulting expression, not the
             * value *)

            ignore (Queue.fold (fun args param ->
              match args with
              | arg::rest ->
                begin
                  if param.parameter_is_var then
                  begin
                    Queue.push (getref arg) call.call_args;
                    rest
                  end
                  else
                  begin
                    Queue.push arg call.call_args;
                    rest
                  end
                end
              | [] -> failwith "[semant] internal error")
              args proc.procedure_parameters);

            (* If the called function has free variables, we will
             * have to add them as well to the caller, so record
             * this for later analysis. We can't compute free
             * variables here because if we have a set of recursive
             * functions, we would loop endlessly if we are not
             * careful *)

            add_free_var (FreeCall proc);

            (* Also we record every call into [proc]'s entry
             * in the symbol table. This is used later for
             * adding free variables as explicit parameters
             * in module Lift *)

            Queue.push call proc.procedure_calls;

            Call call
        end
      | Variable _ ->
          failwith "[semant] can't call variable"
      | Parameter _ ->
          failwith "[semant] can't call param"
      | Constant _ ->
          failwith "[semant] can't call constant"
      | Type _ ->
          failwith "[semant] can't call a type"
      with Not_found ->
        failwith "[semant] procedure not found"
    end
  | Ast.If (cond, body, elsifs, els) ->
      let cond = expression_with_type lvl env
        add_free_var cond Types.boolean
      in let body = List.map (statement lvl env add_free_var) body
      in let elsifs = List.map (fun (cond, body) ->
          let cond = expression_with_type lvl env
            add_free_var cond Types.boolean
          in let body = List.map (statement lvl env add_free_var)
            body
          in (cond, body)) elsifs
      in
        If ((cond, body) :: elsifs,
          List.map (statement lvl env add_free_var) els)

  | Ast.While (cond, body) ->
      let cond = expression lvl env add_free_var cond in
        if not (Types.equal cond.expression_type Types.boolean) then
          failwith "while condition is not of type boolean"
        else
          While (cond, List.map (statement lvl env add_free_var) body)

let base_env () =
  let env = Env.create () in
    Env.add env "INTEGER" (Type Types.integer);
    Env.add env "BOOLEAN" (Type Types.boolean);
    env

let constant n v =
  {
    constant_name = n;
    constant_value = v;
  }

let rec lookup_type env = function
  | Ast.TypeName n ->
    begin
      try match Env.find env n with
      | Type ti -> ti
      | _ -> failwith "not a type"
      with Not_found -> failwith "type not found"
    end
  | Ast.TypeArray (size, t) ->
      {
        Types.type_name = "";
        Types.type_desc =
          Types.Array (evaluate_expression env size, lookup_type env t);
      }
  | Ast.TypeRecord fields ->
      {
        Types.type_name = "";
        Types.type_desc =
          Types.Record (List.map (fun (n, t) ->
            (n, lookup_type env t)) fields);
      }

let variable lvl env n t =
  {
    variable_name = n;
    variable_level = lvl;
    variable_type = lookup_type env t;
  }

let rec block lvl env add_free_var blk =
  let lvl = lvl + 1 in

  (* Open a new scope in the environment.
   * This will be discarded once we are done
   * with this block *)

  let env = Env.scope env in

  let add_const (n, v) =
    Env.add env n (Constant (constant n v))
  in

  let add_var (n, t) =
    let var = variable lvl env n t in
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
      List.iter (fun (is_var, name, t) ->
        Queue.push
          {
            parameter_name = name;
            parameter_level = lvl+1;
            parameter_is_var = is_var;
            parameter_type = lookup_type env t;
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
          Queue.push v prc.procedure_free_variables
        in

          (* Add the parameters to the environment
           * before going over the procedure's body *)

          Queue.iter (fun param ->
            Env.add env param.parameter_name (Parameter param))
            prc.procedure_parameters;

          (* The procedure's variables will be added in
           * the recursive call to [block] *)

          prc.procedure_block <-
            Some (block lvl env add_free_var proc.Ast.procedure_block)
      end
    | _ ->
        failwith "[semant] internal error"
    with Not_found ->
      failwith "[semant] internal error"
  in

    (* Add the block's constants *)

    List.iter add_const blk.Ast.block_constants;

    (* Add the block's local variables *)

    let vars = List.map add_var blk.Ast.block_variables in

    (* TODO Add the block's local procedures. We add all
     * the headers before going over the bodies
     * in order to allow recursive calls. Later probably
     * we will only do this for forward procedures *)

    let procs = List.map add_proc_header blk.Ast.block_procedures in
      List.iter add_proc blk.Ast.block_procedures;

      (* Analyze the block's body in the resulting environment *)

      let stmt = List.map (statement lvl env add_free_var) blk.Ast.block_body in
        {
          block_variables = vars;
          block_procedures = procs;
          block_body = stmt;
        }

let oberon_module m =

  (* The third parameter is a function called for free variables,
   * params or calls. It will generally be called for recursive calls
   * on top level *)

  {
    module_name = m.Ast.module_name;
    module_block = block (-1) (base_env ()) (fun _ -> ()) m.Ast.module_block;
  }
