type 'a data = {
  mutable count : int;
  mutable desc : 'a array;
}

type 'a t = 'a data ref

let length seq =
  Array.length !seq.desc

let set seq idx x =
  if !seq.count > 1 then
  begin
    !seq.count <- !seq.count - 1;
    seq := { count = 1; desc = Array.copy !seq.desc }
  end;
  !seq.desc.(idx) <- x

let get seq idx =
  !seq.desc.(idx)

let copy seq =
  !seq.count <- !seq.count + 1;
  ref !seq

let create n x =
  ref { count = 1; desc = Array.make n x }
