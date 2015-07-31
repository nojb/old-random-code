open Str
open Printf

let loc = ref 0

let opcode_regexp = regexp "\\([A-Z0-9]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\(!?\\)\\([0-9]+\\)[ \t]+\\([0-9]+\\)[ \t]*\\({\\(.*\\)}\\)?.*$"

type inst = {
    name : string;
    opcode : int;
    uses_field : bool;
    default_field : int;
    time : int;
    code : string;
}

let opcodes : inst Queue.t = Queue.create ()

let load_opcodes ic =
    try while true do let s = input_line ic in
        incr loc;
        if string_match opcode_regexp s 0 then
            Queue.add {
                name = matched_group 1 s;
                opcode = int_of_string (matched_group 2 s);
                uses_field = (matched_group 3 s <> "");
                default_field = int_of_string (matched_group 4 s);
                time = int_of_string (matched_group 5 s);
                code = try matched_group 7 s with Not_found -> " () ";
            }  opcodes
        else
            printf "%d: illegal syntax; ignoring.\n" !loc
    done with End_of_file -> ()

let load_file name =
    loc := 0;
    Queue.clear opcodes;
    let ic = open_in name in
        load_opcodes ic;
        printf "%d lines in file `%s', %d opcodes read successfuly.\n" !loc name (Queue.length opcodes)

let dump_opcodes () =
    Queue.iter (fun h -> printf "%4s(%d)\t%3d\t%3d\t%c\t{%s}\n" h.name h.default_field h.opcode h.time (if h.uses_field then '+' else ' ') h.code) opcodes

let _ =
    load_file "opcodes";
    dump_opcodes ()
