type 'a r =
  | Empty
  | Cons of 'a * 'a t

and 'a t =
  'a r Lazy.t

let rec map f zlst =
  lazy (match Lazy.force zlst with
  | Empty -> Empty
  | Cons (x, y) -> Cons (f x, map f y))

let rec map2 f zlst1 zlst2 =
  lazy (match Lazy.force zlst1, Lazy.force zlst2 with
  | Empty, Empty -> Empty
  | Cons (x1, y1), Cons (x2, y2) ->
      Cons (f x1 x2, map2 f y1 y2)
  | _ -> failwith "map2")

let is_empty zlst =
  match Lazy.force zlst with
  | Empty -> true
  | Cons _ -> false

let rec append zlst1 zlst2 =
  lazy (match Lazy.force zlst1 with
  | Empty -> Lazy.force zlst2
  | Cons (x, y) -> Cons (x, append y zlst2))

let cons x y =
  lazy (Cons (x, Lazy.force y))

let hd zlst =
  match Lazy.force zlst with
  | Empty -> failwith "hd"
  | Cons (x, _) -> x

let tl zlst =
  match Lazy.force zlst with
  | Empty -> failwith "tl"
  | Cons (_, y) -> y

let rec filter p zlst =
  lazy (match Lazy.force zlst with
  | Empty -> Empty
  | Cons (x, y) ->
      if p x then Cons (x, filter p y)
      else Lazy.force (filter p y))

let rec nth n zlst =
  if n = 0 then hd zlst
  else nth (n-1) (tl zlst)

let rec interleave s1 s2 =
  lazy (match Lazy.force s1 with
  | Empty -> Lazy.force s2
  | Cons (x, y) ->
      Cons (x, interleave s2 y))

let rec flatten s =
  lazy (match Lazy.force s with
  | Empty -> Empty
  | Cons (x, y) ->
      Lazy.force (interleave x (flatten y)))

let flatmap f s =
  flatten (map (map f) s)

let empty =
  lazy (Empty)

let singleton x =
  lazy (Cons (x, lazy Empty))
