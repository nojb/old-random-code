open Printf
open Interp

let string_of_fl x =
  let n = int_of_float x in
  if float_of_int n = x then
    (string_of_int n)
  else
    (string_of_float x)

let register vm =

Hashtbl.add vm.float_variables "E" (exp 1.0);
Hashtbl.add vm.float_variables "PI" (4.0 *. atan 1.0);

register_fun_i_s vm "CHR" (fun x -> sprintf "%c" (Char.chr x));
register_fun_f_f vm "COS" cos;
register_fun_f_f vm "SQRT" sqrt;
register_fun_f_f vm "ACOS" acos;
register_fun_f_f vm "ASIN" asin;
register_fun_f_f vm "ATAN" atan;
register_fun_ff_f vm "POW"
  (fun x y -> x ** y);
register_fun_f_f vm "INT"
  (fun x -> float_of_int (int_of_float x));
register_fun_si_s vm "LEFT"
  (fun s i -> let i = min i (String.length s) in
    String.sub s 0 i);
register_fun_si_s vm "RIGHT"
  (fun s i -> let i = min i (String.length s) in
    String.sub s ((String.length s)-i) i);
register_fun_f_s vm "STR"
  (fun x -> string_of_fl x);
register_fun_si_s vm "STRNG"
  (fun s i ->
    let rec loop i result =
      if i = 0 then result
      else loop (i-1) (result ^ s)
    in loop (i-1) s);
register_fun_s_f vm "VAL"
  (fun s -> float_of_string s);
register_fun_s_i vm "LEN" (fun s -> String.length s);
register_fun_f_f vm "LN" log;
register_fun_ssi_i vm "INSTR"
  (fun s1 s2 i ->
    let l2 = String.length s2 in
    let rec loop j k =
      if j = l2 then (k+1)
      else if s1.[k+j] = s2.[j] then
        loop (j+1) k
      else loop 0 (k+1)
    in loop 0 0);
register_fun_sii_s vm "MID"
  (fun s start len ->
    if len < 0 then String.sub s (start-1) ((String.length s)-start+1)
    else let len = min len (String.length s) in
      String.sub s (start-1) len);
register_fun_s_f vm "SHELL"
  (fun cmd -> float_of_int (Sys.command cmd));
register_fun_s_i vm "ASCII"
  (fun x -> Char.code x.[0]);
