open Ocamlbuild_plugin;;

ocaml_lib ~extern:true "llvm";;

flag ["link"; "ocaml"; "g++"] (S[A"-cc"; A"g++"]);;
