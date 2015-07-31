type subtitle =
  {
    number : int;
    start : Time.t;
    finish : Time.t;
    text : string;
  }

let skip_and_input_line ic =
  let rec loop l =
    if l = "\r" then loop (input_line ic)
    else l
  in loop (input_line ic)

let read_text ic =
  let buf = Buffer.create 20 in
  let rec loop p =
    if p = "\r" then
      Buffer.contents buf
    else
      begin
        Buffer.add_string buf p;
        Buffer.add_char buf '\n';
        loop (input_line ic)
      end
  in loop (skip_and_input_line ic)

let read ic =
  let n = Scanf.sscanf (skip_and_input_line ic) "%d\r" (fun x -> x) in
  let times = skip_and_input_line ic in
  let (start, finish) =
    Scanf.sscanf times "%12s --> %12s\r"
      (fun start finish -> Time.of_string start, Time.of_string finish)
  in
  let text = read_text ic in
    {
      number = n;
      start = start;
      finish = finish;
      text = text;
    }

let write oc sub =
  Printf.fprintf oc "%d\r\n%s --> %s\r\n%s\r\n"
    sub.number
    (Time.to_string sub.start)
    (Time.to_string sub.finish)
    sub.text

let read_channel ic =
  let lst = ref [] in
  try while true do
    lst := (read ic) :: !lst
  done; raise End_of_file
  with End_of_file -> Array.of_list (List.rev !lst)

let read_all ic =
  let rec loop lst =
    try loop ((read ic)::lst)
    with End_of_file -> Array.of_list (List.rev lst)
  in loop []

let scale_channel ic oc y1 y2 =
  let subs = read_channel ic in
  let last = subs.((Array.length subs)-1) in
  let first = subs.(0) in
    Array.iter (fun s ->
      write oc
        {
          s with
            start =
              Time.scale (first.start,last.start) (y1,y2) s.start;
            finish =
              Time.scale (first.start,last.start) (y1,y2) s.finish;
        }) subs
