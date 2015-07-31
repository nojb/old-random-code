open Printf
open Pcode
open Error

type value =
  | String of string
  | Float of float
  | StringVector of string MultiArray.t
  | FloatVector of float MultiArray.t

let rec string_of_value v =
  match v with
  | String s -> s
  | Float x -> string_of_fl x
  | StringVector v -> ""
  | FloatVector v -> ""

  (* should throw TYPEMISMATCH if x is not an actual integer? *)
let int_value v =
  match v with
  | Float x -> int_of_float x
  | _ -> fail TYPEMISMATCH

let float_value v =
  match v with
  | Float x -> x
  | _ -> fail TYPEMISMATCH

let string_value v =
  match v with
  | String s -> s
  | _ -> fail TYPEMISMATCH

let add_values v1 v2 =
  match v1, v2 with
  | String s1, String s2 -> String (s1 ^ s2)
  | Float x1, Float x2 -> Float (x1 +. x2)
  | _ -> fail TYPEMISMATCH

let sub_values v1 v2 =
  match v1, v2 with
  | Float x1, Float x2 -> Float (x1 -. x2)
  | _ -> fail TYPEMISMATCH

let mul_values v1 v2 =
  match v1, v2 with
  | Float x1, Float x2 -> Float (x1 *. x2)
  | _ -> fail TYPEMISMATCH

let div_values v1 v2 =
  match v1, v2 with
  | Float x1, Float x2 -> Float (x1 /. x2)
  | _ -> fail TYPEMISMATCH

let mod_values v1 v2 =
  match v1, v2 with
  | Float v1, Float v2 ->
      Float (float_of_int
        ((int_of_float v1) mod (int_of_float v2)))
  | _ -> fail TYPEMISMATCH

let pow_values v1 v2 =
  match v1, v2 with
  | Float x1, Float x2 -> Float (x1 ** x2)
  | _ -> fail TYPEMISMATCH

let compare_values flop sop v1 v2 =
  match v1, v2 with
  | Float v1, Float v2 -> if flop v1 v2 then Float 1.0 else Float 0.0
  | String s1, String s2 -> if sop s1 s2 then Float 1.0 else Float 0.0
  | _ -> fail TYPEMISMATCH
