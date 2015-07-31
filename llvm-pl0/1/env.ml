module Tbl = Map.Make(
  struct
    type t = string
    let compare x y =
      compare (String.lowercase x) (String.lowercase y)
  end)

type 'a t = 'a Tbl.t ref Stack.t

let add env k v = env := Tbl.add k v !env
let find env k = Tbl.find k !env
