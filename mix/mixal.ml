open Vm
open Globals
open Scanner

let _ =
	scan_program ();
	load_program ();
	run_program ()
