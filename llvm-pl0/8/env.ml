module Tbl = Map.Make
(
  struct
    type t = string
    let compare x y =
      compare (String.lowercase x) (String.lowercase y)
  end
)

type 'a t = 'a Tbl.t ref

let create () = ref Tbl.empty
let add tbl key x = tbl := Tbl.add key x !tbl
let find tbl key = Tbl.find key !tbl
let remove tbl key = tbl := Tbl.remove key !tbl
let scope tbl = ref !tbl
