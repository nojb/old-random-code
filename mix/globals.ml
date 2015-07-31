let loc = ref 0
let lineno = ref 0
let lab = ref ""
let op = ref ""
let addr = ref ""
let linecount = ref 0

let curr_loc () = !loc
let set_loc x = loc := x
let incr_loc () = incr loc
let clear_loc () = loc := 0

let incr_lineno () = incr lineno
let curr_lineno () = !lineno
let clear_lineno () = lineno := 0
let set_lineno n = lineno := n

let set_linecount n = linecount := n
let clear_linecount () = linecount := (-1)

let set_line x1 x2 x3 = lab := x1; op := x2; addr := x3

let tbl = Hashtbl.create 200

let here_tbl = ref []

let here_label_regexp = Str.regexp "[0-9][hH]"
let here_ref_regexp = Str.regexp "[0-9][fFbB]"

let is_here_label s = Str.string_match here_label_regexp s 0
let is_here_sym s = Str.string_match here_ref_regexp s 0

let add_sym s x =
  if s <> "" then begin
    if is_here_label s then begin
      here_tbl := (!lineno, s.[0], !loc)::!here_tbl;
      Printf.eprintf "%d: added HERE label %s @ %d.\n" !lineno s !loc
    end else Hashtbl.add tbl s x
  end

let lookup_here_label s =
  let rec help l last =
    match l with
    | [] -> if s.[1] = 'f' || s.[1] = 'F' then (match last with None -> raise Not_found | Some (lin2,c2,adr2) ->
        begin Printf.eprintf "%d: HERE symbol `%s' refers to HERE label %cH at line %d @ %d\n" !lineno s c2 lin2 adr2; adr2 end) else raise Not_found
    | (lin,c,adr)::k -> if c <> s.[0] then help k last
      else begin
	if lin < !lineno then begin
	  if s.[1] = 'B' || s.[1] = 'b' then begin
	    Printf.eprintf "%d: HERE symbol `%s' refers to HERE label %cH at line %d @ %d\n" !lineno s c lin adr;
	    adr
	  end else (match last with None -> raise Not_found | Some (lin2,c2,adr2) -> begin
		Printf.eprintf "%d: HERE symbol `%s' refers to HERE label %cH at line %d @ %d\n" !lineno s c2 lin2 adr2;
		adr2 end)
	end else help k (Some (lin,c,adr))
      end
  in help !here_tbl None

let find_sym s =
  try if is_here_sym s then lookup_here_label s
    else let v = Hashtbl.find tbl s in begin (*Printf.printf "FIND_SYM `%s' = %d\n" s v;*) v end
  with Not_found -> (Printf.eprintf "%d: symbol `%s' undefined; assigning value 0.\n" !lineno s; 0)

let clear_symbols () = Hashtbl.clear tbl; here_tbl := []

let dump_symbols oc =
  Printf.fprintf oc "---- SYMBOL TABLE ----\n";
  Hashtbl.iter (fun s x -> Printf.fprintf oc "%10s\t\t%d\n" s x) tbl

type mixline = {
  text : string * string * string;
  lineno : int;
  address : int;
  mutable asm : int;
};;

let program : mixline Queue.t = Queue.create ()
let push_line x = Queue.push { text = (!lab,!op,!addr); lineno = !lineno; address = !loc; asm = x } program
let clear_program () = Queue.clear program

let dump_mixline oc { text = (x1,x2,x3); lineno = no; address = a; asm = x } =
  Printf.fprintf oc "%04d @ %04d [%s]: [%10s] %5s \"%s\"\n" no a (Words.to_string x) x1 x2 x3

let dump_program oc =
  Printf.fprintf oc "---- PROGRAM DUMP ----\n";
  Queue.iter (dump_mixline oc) program

let end_address = ref (-1)
let set_end_address n = end_address := n
let start_address = ref 0

let signal_end adr =
  start_address := adr;
  end_address := !loc

let literals : (int * int) Queue.t = Queue.create ()

let add_literal x =
  let u = !end_address in
  Queue.add (x,u) literals;
  incr end_address;
  u

let push_literals () =
  Queue.iter (fun (x,u) -> Queue.push { text = ("","CON",""); lineno = (-1); address = u; asm = x } program) literals
