type t = {
  mutable abso : int;
  mutable sign : bool;
};;

let sign_mask = 0b1_000000_000000_000000_000000_000000
let abso_mask = lnot sign_mask

let positive_zero = { abso = 0; sign = false }
;;

let sign x = x.sign
;;

let neg w = { abso = w.abso; sign = not w.sign }

let of_int n = { abso = abs n; sign = (n < 0) }
;;

let to_int w =
  if w.sign then -(w.abso)
  else w.abso
;;

let get_field w i j =
  if (i <= j) && (0 <= i) && (j <= 5) then begin
    let k = (max i 1)-1 in
    let mask1 = ((-1) lsl ((5-j)*6)) in
    let mask2 = (abso_mask lsr (k*6)) in
    let new_abso = (w.abso land mask1 land mask2) lsr ((5-j)*6) in
      { abso = new_abso; sign = (if i == 0 then w.sign else false) }
  end else { abso = 0; sign = false }
;;

let get_fieldi w i j = to_int (get_field w i j)

let get_fieldp w f = get_field w (f/8) (f%8)

let setfield w i j w2 =
  if (i <= j) && (0 <= i) && (j <= 5) then begin
    let k = (max i 1)-1 in
    let mask1 = 
  end
;;

let setfieldi w i j n =
  setfield w i j (of_int n)
;;

let to_string w =
  Printf.sprintf "%s %02d %02d %02d %02d %02d"
    (if w.sign then "-" else "+") (get_fieldi w 1 1) (get_fieldi w 2 2)
    (get_fieldi w 3 3) (get_fieldi w 4 4) (get_fieldi w 5 5)
