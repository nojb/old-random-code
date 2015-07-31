type mat_exp =
  | MATVAR of string
  | MATBIN of mat_exp * binop * mat_exp
  | MATINV of string
  | MATTRN of string
  | MATFLOAT of float
  | MATZER of exp * exp
  | MATIDN of exp
