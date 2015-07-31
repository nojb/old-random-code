type t =
  {
    hours : int;
    minutes : int;
    seconds : int;
    milliseconds : int;
  }

let to_float t =
  let h = float_of_int t.hours in
  let m = float_of_int t.minutes in
  let s = float_of_int t.seconds in
  let ms = float_of_int t.milliseconds in
    ms *. 0.001 +. s +. m *. 60.0 +. h *. 3600.0

let of_float x =
  let h = int_of_float (x /. 3600.0) in
  let x = x -. ((float_of_int h) *. 3600.0) in
  let m = int_of_float (x /. 60.0) in
  let x = x -. ((float_of_int m) *. 60.0) in
  let s = int_of_float x in
  let x = x -. (float_of_int s) in
  let ms = int_of_float (x *. 1000.0) in
    {
      hours = h;
      minutes = m;
      seconds = s;
      milliseconds = ms;
    }

let of_string s =
  Scanf.sscanf s "%2d:%2d:%2d,%3d" (fun h m s ms ->
    {
      hours = h;
      minutes = m;
      seconds = s;
      milliseconds = ms;
    })

let to_string t =
  Printf.sprintf "%02d:%02d:%02d,%03d" t.hours t.minutes t.seconds t.milliseconds

let scale (srcstart,srcfinish) (dststart,dstfinish) t =
  let x1 = to_float srcstart in
  let x2 = to_float srcfinish in
  let y1 = to_float dststart in
  let y2 = to_float dstfinish in
  let z = to_float t in
  let m = (y2 -. y1) /. (x2 -. x1) in
  let b = y1 -. (x1 *. m) in
    (* Printf.printf "m = %f, b = %f\n" m b; *)
    of_float ((m *. z) +. b)

(* let x1 = of_string "00:00:00,000"
let x2 = of_string "04:00:00,000"
let y1 = of_string "01:00:00,000"
let y2 = of_string "02:00:00,000"

let r = of_string "01:00:00,000" *)
(* should scale (x1,x2) (y1,y2) r => 01:15:00,000 *)
