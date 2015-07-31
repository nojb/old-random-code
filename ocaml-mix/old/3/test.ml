let asm orig instructions =
  let vm = Vm.make () in
  for i = 0 to (Array.length instructions)-1 do
    let inst = Scanf.sscanf instructions.(i) "%c %d %d %d %d"
      (fun sign b12 b3 b4 b5 ->
        [|if sign = '+' then 1 else -1; (b12 lsr 6) land 0x3f; b12 land 0x3f; b3; b4; b5|])
    in vm.Vm.memory.(orig+i) <- inst
  done;
  vm


(*let main () =
  let orig = read_int () in
  let rec loop lst =
    try loop ((read_line ())::lst)
    with End_of_file -> Array.of_list (List.rev lst)
  in Vm.run (asm orig (loop [])) orig
  *)

let main () =
  let vm = Vm.make () in
  try let rec loop i =
    let inp = read_line () in
    let inst = Scanf.sscanf inp "%c %d %d %d %d"
      (fun sign b12 b3 b4 b5 ->
        [|if sign = '+' then 1 else -1; (b12 lsr 6) land 0x3f; b12 land 0x3f; b3; b4; b5|])
    in
      vm.Vm.memory.(i) <- inst;
      Vm.step vm;
      loop (i+1)
  in loop 0
  with End_of_file -> ()

let _ = main ()
