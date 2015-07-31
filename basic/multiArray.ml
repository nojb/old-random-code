exception Bad_argument
  
type 'a t =
| Simple of 'a array
| Multi of 'a t array

let rec make il x =
  match il with
  | a::[] -> Simple (Array.make a x)
  | a::b -> Multi (Array.make a (make b x))
  | [] -> assert false

let rec dimension x =
  match x with
  | Simple _ -> 1
  | Multi x -> begin
    try 1 + dimension (x.(0))
    with Invalid_argument _ -> raise Bad_argument
  end

let rec length x =
  match x with
  | Simple x -> [Array.length x]
  | Multi x -> begin
    try (Array.length x)::(length x.(0))
    with Invalid_argument _ -> raise Bad_argument
  end

let rec get x idx =
  match x, idx with
  | Simple x, a::[] -> begin
    try x.(a)
    with Invalid_argument _ -> raise Bad_argument
  end
  | Multi x, a::b -> get x.(a) b
  | _ -> raise Bad_argument

let rec set x idx v =
  match x, idx with
  | Simple x, a::[] -> begin
    try x.(a) <- v
    with Invalid_argument _ -> raise Bad_argument
  end
  | Multi x, a::b -> set x.(a) b v
  | _ -> raise Bad_argument

(*let rec to_list x =
  match x with
  | Simple x -> Array.to_list x
  | Multi x -> Array.to_list (Array.map to_list x)
  *)

let rec map f x =
  match x with
  | Simple x -> Array.map f x
  | Multi x -> Array.map f (Array.map (map f) x)
