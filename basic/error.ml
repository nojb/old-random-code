open Printf

type error_t =
  | UNDEFINED of string
  | ILLEGALRETURN
  | ILLEGALLINE of int
  | SUBSCRIPTERROR
  | TYPEMISMATCH
  | NEXTWITHOUTFOR
  | FORWITHOUTNEXT
  | OTHER of string
  | NODATA

type error_desc = {
  error_type : error_t;
  error_line : int option;
}
  
exception ERROR of error_desc

let fail x = raise (ERROR { error_type = x; error_line = None })
let fail_at lab x = raise (ERROR { error_type = x; error_line = Some lab })
let fail_because x = raise (ERROR { error_type = OTHER x; error_line = None })

let string_of_error_t e =
  match e with
  | UNDEFINED s -> sprintf "UNDEFINED %s" s
  | ILLEGALRETURN -> "ILLEGAL RETURN"
  | ILLEGALLINE i -> sprintf "ILLEGAL LINE %d" i
  | SUBSCRIPTERROR -> "SUBSCRIPT ERROR"
  | TYPEMISMATCH -> "TYPE MISMATCH"
  | NEXTWITHOUTFOR -> "NEXT without FOR"
  | FORWITHOUTNEXT -> "FOR without NEXT"
  | OTHER s -> s
  | NODATA -> "NO DATA"

let fail_explain desc =
  match desc.error_line with
  | None ->
    print_endline (string_of_error_t desc.error_type)
  | Some lab ->
    printf "%d %s\n" lab (string_of_error_t desc.error_type)

