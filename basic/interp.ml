open Printf
open Pcode
open Runtime
open Error

module DynArray = Extlib.DynArray

exception Stop

type for_data = {
  for_variable : string;
  for_next : int;
  for_finish : exp;
  for_step : exp;
}

type read_t = {
  mutable read_index : int;
  mutable read_data : value DynArray.t;
}

type vm = {
  program : (int * cmd) DynArray.t;
  mutable name : string;

  string_variables : (string, string) Hashtbl.t;
  float_variables : (string, float) Hashtbl.t;
  stringvector_variables : (string, string MultiArray.t) Hashtbl.t;
  floatvector_variables : (string, float MultiArray.t) Hashtbl.t;

  string_functions : (string, (value list -> string)) Hashtbl.t;
  float_functions : (string, (value list -> float)) Hashtbl.t;

  substack : int Stack.t;
  forstack : for_data Stack.t;
  userstack : value Stack.t;

  data : read_t;

  mutable halted : bool;
  mutable pc : int;
}

let store_line vm i c =
  try let idx = DynArray.index_of (fun (lab,_) -> lab >= i) vm.program in
  let (lab,_) = DynArray.get vm.program idx in
  if lab = i then DynArray.set vm.program idx (i,c)
  else begin
    DynArray.insert vm.program idx (i,c);
    if vm.pc >= idx then vm.pc <- vm.pc + 1
  end
  with Not_found -> DynArray.add vm.program (i,c)

let delete_line vm i =
  try DynArray.delete vm.program
    (DynArray.index_of (fun (lab,_) -> lab = i) vm.program)
  with Not_found -> ()

let pop_data vm =
  try let x = DynArray.get vm.data.read_data vm.data.read_index in
    vm.data.read_index <- vm.data.read_index + 1;
    x
  with DynArray.Invalid_arg _ -> fail NODATA

let push_data vm x =
  DynArray.add vm.data.read_data x

let list_program out vm =
  DynArray.iter (fun (lab, x) -> fprintf out "%-5d %s\n" lab (string_of_cmd x))
    vm.program

let make_vm () = {
  program = DynArray.create ();
  name = "";

  string_variables = Hashtbl.create 30;
  float_variables = Hashtbl.create 30;
  stringvector_variables = Hashtbl.create 30;
  floatvector_variables = Hashtbl.create 30;

  string_functions = Hashtbl.create 30;
  float_functions = Hashtbl.create 30;

  substack = Stack.create ();
  forstack = Stack.create ();
  userstack = Stack.create ();

  data = { read_index = 0; read_data = DynArray.create () };

  halted = true;
  pc = 0;
}

let lookup_function vm v =
  match v with
  | STRINGVAR v -> begin
    try let f = Hashtbl.find vm.string_functions v in
      (fun vl -> String (f vl))
    with Not_found -> fail (UNDEFINED v)
  end
  | FLOATVAR v -> begin
    try let f = Hashtbl.find vm.float_functions v in
      (fun vl -> Float (f vl))
    with Not_found -> fail (UNDEFINED v)
  end

let lookup_var vm v =
  match v with
  | STRINGVAR v -> begin
    try String (Hashtbl.find vm.string_variables v)
    with Not_found -> fail (UNDEFINED v)
  end
  | FLOATVAR v -> begin
    try Float (Hashtbl.find vm.float_variables v)
    with Not_found -> fail (UNDEFINED v)
  end

let lookup_vector vm v =
  match v with
  | STRINGVAR v -> begin
    try StringVector (Hashtbl.find vm.stringvector_variables v)
    with Not_found -> fail (UNDEFINED v)
  end
  | FLOATVAR v -> begin
    try FloatVector (Hashtbl.find vm.floatvector_variables v)
    with Not_found -> fail (UNDEFINED v)
  end

let eval_binop v1 op v2 =
  match op with
  | ADD -> add_values v1 v2
  | SUB -> sub_values v1 v2
  | MUL -> mul_values v1 v2
  | DIV -> div_values v1 v2
  | POW -> pow_values v1 v2
  | EQ -> compare_values (=) (=) v1 v2
  | NEQ -> compare_values (!=) (!=) v1 v2
  | LE -> compare_values (<=) (<=) v1 v2
  | GE -> compare_values (>=) (>=) v1 v2
  | MOD -> mod_values v1 v2
  | _ -> fail_because (sprintf "binop %s not handled" (string_of_binop op))

let rec eval_ref vm r =
  match r with
  | VAR v -> lookup_var vm v
  | SUBSCRIPT (v,el) -> begin
    try
    match lookup_vector vm v with
    | FloatVector v -> Float
      (MultiArray.get v (List.map (fun e -> int_value (eval_exp vm e)) el))
    | StringVector v -> String
      (MultiArray.get v (List.map (fun e -> int_value (eval_exp vm e)) el))
    | _ -> assert false
    with
    | MultiArray.Bad_argument -> fail SUBSCRIPTERROR
    | ERROR { error_type = UNDEFINED _; error_line = _ } -> begin
      (lookup_function vm v) (List.map (eval_exp vm) el)
    end
  end

and eval_exp vm e =
  match e with
  | FLOAT x -> Float x
  | STRING s -> String s
  | REF r -> eval_ref vm r
  | BIN (e1, op, e2) -> eval_binop (eval_exp vm e1) op (eval_exp vm e2)

let find_label vm i =
  try DynArray.index_of (fun (lab,_) -> lab = i) vm.program
  with Not_found -> fail (ILLEGALLINE i)

let assign_var vm v z =
  match v, z with
  | STRINGVAR v, String z -> Hashtbl.replace vm.string_variables v z
  | FLOATVAR v, Float z -> Hashtbl.replace vm.float_variables v z
  | _ -> fail TYPEMISMATCH

let string_of_option x =
  match x with
  | None -> ""
  | Some x -> x

let eval_let vm r z =
  match r with
  | VAR v -> assign_var vm v z
  | SUBSCRIPT (v, el) -> begin
    try
    match (lookup_vector vm v), z with
    | FloatVector v, Float x ->
      MultiArray.set v (List.map (fun e -> int_value (eval_exp vm e)) el) x
    | StringVector v, String s ->
      MultiArray.set v (List.map (fun e -> int_value (eval_exp vm e)) el) s
    | _ -> fail TYPEMISMATCH
    with MultiArray.Bad_argument -> fail SUBSCRIPTERROR
  end

let rec exec vm cmd =
  match cmd with
  | READ vl ->
    List.iter (fun x -> assign_var vm x (pop_data vm)) vl
  | STOP -> raise Stop
  | INPUT (x, LINE s) -> begin
    print_string (string_of_option x);
    Hashtbl.replace vm.string_variables s (read_line ())
  end
  | INPUT (x, FLOATS fl) -> begin
    print_string (string_of_option x);
    List.iter
      (fun x -> Hashtbl.replace vm.float_variables x (read_float ()))
      fl
  end
  | REM s -> ()
  | PRINT el -> begin
    let help x =
      match x with
      | TAB -> printf "\t"
      | NOSPACE -> ()
      | EXP e -> print_string (string_of_value (eval_exp vm e))
    in let rec loop x =
      match x with
      | a::[] -> begin
        help a;
        match a with
        | EXP _ -> print_newline ()
        | _ -> ()
      end
      | a::b -> (help a; loop b)
      | [] -> print_newline ()
    in loop el; flush stdout
  end
  | GOTO i -> begin
    vm.pc <- find_label vm i
  end
  | GOSUB i -> begin
    Stack.push vm.pc vm.substack;
    vm.pc <- find_label vm i
  end
  | RETURN -> begin
    try vm.pc <- Stack.pop vm.substack;
    with Stack.Empty -> fail ILLEGALRETURN
  end
  | DATA vl ->
    List.iter (push_data vm) (List.map (eval_exp vm) vl)
  | END -> vm.halted <- true
  | IF (e, c) ->
    if int_value (eval_exp vm e) != 0 then
      exec vm c
  | LET (r, e) -> eval_let vm r (eval_exp vm e)
  | CALL (v, el) -> begin
    try ignore ((lookup_function vm v) (List.map (eval_exp vm) el))
    with Not_found -> fail (UNDEFINED (var_name v))
  end
  | FOR (v, start, finish, None) ->
      exec vm (FOR (v, start, finish, Some (FLOAT 1.0)))
  | FOR (v, start, finish, Some stp) -> begin
    let startv = eval_exp vm start in
    let startx = float_value startv in
      assign_var vm (FLOATVAR v) startv;
      if startx <= float_value (eval_exp vm finish) then
        Stack.push
          { for_variable = v;
            for_finish = finish;
            for_step = stp;
            for_next = vm.pc }
          vm.forstack
      else begin
        let rec loop x =
          if vm.pc >= DynArray.length vm.program then
            fail FORWITHOUTNEXT;
          let (_,cmd) = DynArray.get vm.program vm.pc in
          vm.pc <- vm.pc + 1;
          match cmd with
          | FOR (w,_,_,_) -> loop (w::x)
          | NEXT None -> begin
            match x with
            | [] -> ()
            | a::b -> loop b
          end
          | NEXT (Some w) -> begin
            if w != v then
              let rec help (u : string list) =
                match u with
                | z::zl -> if z = w then zl else z::(help zl)
                | [] -> []
              in loop (help x)
          end
          | _ -> loop x
        in loop []
      end
  end
  | NEXT None -> begin
    try exec vm (NEXT (Some (Stack.top vm.forstack).for_variable))
    with Stack.Empty -> fail NEXTWITHOUTFOR
  end
  | NEXT (Some x) -> begin
    try
      Stack.iter (fun data -> if data.for_variable = x then begin
        let currx = Hashtbl.find vm.float_variables x in
        let finishx = float_value (eval_exp vm data.for_finish) in
        let stpx = float_value (eval_exp vm data.for_step) in
        let newx = currx +. stpx in
          Hashtbl.replace vm.float_variables x newx;
          if newx <= finishx then
            vm.pc <- data.for_next
          else
            ignore (Stack.pop vm.forstack);
          raise Exit
      end) vm.forstack;
      fail NEXTWITHOUTFOR
    with Exit -> ()
  end
  | DIM al ->
      List.iter
        (fun (v,el) ->
          match v with
          | STRINGVAR v ->
            Hashtbl.replace vm.stringvector_variables v
              (MultiArray.make
                (List.map (fun e -> (int_value (eval_exp vm e))+1) el) "")
          | FLOATVAR v ->
            Hashtbl.replace vm.floatvector_variables v
              (MultiArray.make
                (List.map (fun e -> (int_value (eval_exp vm e))+1) el) 0.0))
        al

and step vm =
  if vm.pc >= DynArray.length vm.program then
    vm.halted <- true
  else begin
  let (lab, cmd) = DynArray.get vm.program vm.pc in begin
    vm.pc <- vm.pc + 1;
    try exec vm cmd
    with
    | Stop -> printf "BREAK %d\n" lab
    | ERROR { error_type = expl; error_line = None } ->
      fail_at lab expl
    | e -> raise e
  end
  end

and run vm start =
  vm.pc <- start;
  vm.halted <- false;
  while not vm.halted do
    step vm
  done

let register_float_function vm name f =
  Hashtbl.replace vm.float_functions name f

let register_fun_i_s vm name f =
  Hashtbl.replace vm.string_functions name
    (function
      | [v] -> f (int_value v)
      | _ -> fail_because (name ^ "ILLEGAL ARG COUNT"))

let register_fun_f_f vm name f =
  register_float_function vm name
    (function
      | [v] -> f (float_value v)
      | _ -> fail_because (name ^ " Bad arg count"))

let register_fun_s_f vm name f =
  register_float_function vm name
    (function
      | [v] -> f (string_value v)
      | _ -> fail_because (name ^ " Bad arg count"))

let register_fun_s_i vm name f =
  register_float_function vm name
    (function
      | [v] -> float_of_int (f (string_value v))
      | _ -> fail_because (name ^ "ILLEGAL ARG COUNT"))

let register_fun_si_s vm name f =
  Hashtbl.replace vm.string_functions name
    (function
      | [v1; v2] -> f (string_value v1) (int_value v2)
      | _ -> fail_because (name ^ "ILLEGAL ARG COUNT"))

let register_fun_sii_s vm name f =
  Hashtbl.replace vm.string_functions name
    (function
      | [v1; v2; v3] ->
        f (string_value v1) (int_value v2) (int_value v3)
      | _ -> fail_because (name ^ "ILLEGAL ARG COUNT"))

let register_fun_ff_f vm name f =
  Hashtbl.replace vm.float_functions name
    (function
      | [v1; v2] -> f (float_value v1) (float_value v2)
      | _ -> fail_because (name ^ "ILLEGAL ARG COUNT"))

let register_fun_f_s vm name f =
  Hashtbl.replace vm.string_functions name
    (function
      | [v] -> f (float_value v)
      | _ -> fail_because (name ^ "ILLEGAL ARG COUNT"))

let register_fun_ssi_i vm name f =
  Hashtbl.replace vm.float_functions name
    (function
      | [v1; v2; v3] ->
        float_of_int (f (string_value v1) (string_value v2)
          (int_value v3))
      | _ -> fail_because (name ^ " ILLEGAL ARG COUNT"))
