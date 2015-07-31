value emit o code = do {
  output_string o "#include <stdio.h>\n";
  output_string o "char cell[30000];";
  output_string o "int main(void)";
  output_string o "{";
  output_string o "char *p = cell;";
  String.iter (fun
    [ '>' -> output_string o "++p;"
    | '<' -> output_string o "--p;"
    | '+' -> output_string o "++*p;"
    | '-' -> output_string o "--*p;"
    | '.' -> output_string o "putchar(*p);"
    | ',' -> output_string o "*p = getchar();"
    | '[' -> output_string o "while(*p){"
    | ']' -> output_string o "}"
    | _ -> () ]) code;
  output_string o "return 0;";
  output_string o "}\n"
};

value read_file i =
  let rec loop s =
    try loop (s ^ (input_line i))
    with [ End_of_file -> s ]
  in loop ""
  ;

value main () =
  emit stdout (read_file stdin)
  ;

main ()
;
