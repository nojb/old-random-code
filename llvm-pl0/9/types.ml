type type_info =
  {
    type_name : string;
    type_desc : type_desc;
  }

and type_desc =
  | Integer
  | Boolean
  | Array of int * type_info
  | Record of (string * type_info) list

let equal t1 t2 = (t1 = t2)

let integer =
  {
    type_name = "INTEGER";
    type_desc = Integer;
  }

let boolean =
  {
    type_name = "BOOLEAN";
    type_desc = Boolean;
  }

let rec to_lltype c t =
  match t.type_desc with
  | Integer -> Llvm.i32_type c
  | Boolean -> Llvm.i1_type c
  | Array (size, t) -> Llvm.array_type (to_lltype c t) size
  | Record fields ->
      Llvm.struct_type c (Array.of_list
        (List.map (fun (_, t) -> to_lltype c t) fields))

let is_array t =
  match t.type_desc with
  | Array _ -> true
  | _ -> false

let array_element_type t =
  match t.type_desc with
  | Array (_, t) -> t
  | _ -> failwith "array_element_type : not an array"
