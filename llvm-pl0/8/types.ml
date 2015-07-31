type type_info =
  {
    type_name : string;
    type_desc : type_desc;
  }

and type_desc =
  | Integer
  | Boolean
  (* | Array of int option * type_info
  | Record of (string * type_info) list *)

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

let to_lltype c t =
  match t.type_desc with
  | Integer -> Llvm.i32_type c
  | Boolean -> Llvm.i1_type c
