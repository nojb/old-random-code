type opcode =
	| NOP | ADD | FADD | SUB | FSUB | MUL | FMUL | DIV | FDIV
	| NUM | CHAR | HLT
	| SLA | SRA | SLAX | SLC | SRAX | SRC
	| MOVE
	| LDA | LD1 | LD2 | LD3 | LD4 | LD5 | LD6 | LDX
	| LDAN | LD1N | LD2N | LD3N | LD4N | LD5N | LD6N | LDXN
	| STA | ST1 | ST2 | ST3 | ST4 | ST5 | ST6 | STX | STJ | STZ
	| JBUS | IOC | IN | OUT | JRED
	| JMP | JSJ | JOV | JNOV | JL | JE | JG | JGE | JNE | JLE
	| JAN | J1N | J2N | J3N | J4N | J5N | J6N | JXN
	| JAZ | J1Z | J2Z | J3Z | J4Z | J5Z | J6Z | JXZ
	| JAP | J1P | J2P | J3P | J4P | J5P | J6P | JXP
	| JANN | J1NN | J2NN | J3NN | J4NN | J5NN | J6NN | JXNN
	| JANZ | J1NZ | J2NZ | J3NZ | J4NZ | J5NZ | J6NZ | JXNZ
	| JANP | J1NP | J2NP | J3NP | J4NP | J5NP | J6NP | JXNP
	| INCA | DECA | ENTA | ENNA
	| INC1 | DEC1 | ENT1 | ENN1
	| INC2 | DEC2 | ENT2 | ENN2
	| INC3 | DEC3 | ENT3 | ENN3
	| INC4 | DEC4 | ENT4 | ENN4
	| INC5 | DEC5 | ENT5 | ENN5
	| INC6 | DEC6 | ENT6 | ENN6
	| INCX | DECX | ENTX | ENNX
	| CMPA | FCMP | CMP1 | CMP2 | CMP3 | CMP4 | CMP5 | CMP6 | CMPX
;;

let opcode_list = [
	("NOP", NOP); ("ADD", ADD); ("FADD", FADD); ("SUB", SUB); ("FSUB", FSUB); ("MUL", MUL); ("FMUL", FMUL); ("DIV", DIV); ("FDIV", FDIV);
	("NUM", NUM); ("CHAR", CHAR); ("HLT", HLT);
	("SLA", SLA); ("SRA", SRA); ("SLAX", SLAX); ("SRAX", SRAX); ("SLC", SLC); ("SRC", SRC);
	("MOVE", MOVE);
	("LDA", LDA); ("LD1", LD1); ("LD2", LD2); ("LD3", LD3); ("LD4", LD4); ("LD5", LD5); ("LD6", LD6); ("LDX", LDX);
	("LDAN", LDAN); ("LD1N", LD1N); ("LD2N", LD2N); ("LD3N", LD3N); ("LD4N", LD4N); ("LD5N", LD5N); ("LD6N", LD6N); ("LDXN", LDXN);
	("STA", STA); ("ST1", ST1); ("ST2", ST2); ("ST3", ST3); ("ST4", ST4); ("ST5", ST5); ("ST6", ST6); ("STX", STX); ("STJ", STJ); ("STZ", STZ);
	("JBUS", JBUS); ("IOC", IOC); ("IN", IN); ("OUT", OUT); ("JRED", JRED);
	("JMP", JMP); ("JSJ", JSJ); ("JOV", JOV); ("JNOV", JNOV); ("JL", JL); ("JE", JE); ("JG", JG); ("JGE", JGE); ("JNE", JNE); ("JLE", JLE);
	("JAN", JAN); ("J1N", J1N); ("J2N", J2N); ("J3N", J3N); ("J4N", J4N); ("J5N", J5N); ("J6N", J6N); ("JXN", JXN);
	("JAZ", JAZ); ("J1Z", J1Z); ("J2Z", J2Z); ("J3Z", J3Z); ("J4Z", J4Z); ("J5Z", J5Z); ("J6Z", J6Z); ("JXZ", JXZ);
	("JAP", JAP); ("J1P", J1P); ("J2P", J2P); ("J3P", J3P); ("J4P", J4P); ("J5P", J5P); ("J6P", J6P); ("JXP", JXP);
	("JANN", JANN); ("J1NN", J1NN); ("J2NN", J2NN); ("J3NN", J3NN); ("J4NN", J4NN); ("J5NN", J5NN); ("J6NN", J6NN); ("JXNN", JXNN);
	("JANZ", JANZ); ("J1NZ", J1NZ); ("J2NZ", J2NZ); ("J3NZ", J3NZ); ("J4NZ", J4NZ); ("J5NZ", J5NZ); ("J6NZ", J6NZ); ("JXNZ", JXNZ);
	("JANP", JANP); ("J1NP", J1NP); ("J2NP", J2NP); ("J3NP", J3NP); ("J4NP", J4NP); ("J5NP", J5NP); ("J6NP", J6NP); ("JXNP", JXNP);
	("INCA", INCA); ("DECA", DECA); ("ENTA", ENTA); ("ENNA", ENNA);
	("INC1", INC1); ("DEC1", DEC1); ("ENT1", ENT1); ("ENN1", ENN1);
	("INC2", INC2); ("DEC2", DEC2); ("ENT2", ENT2); ("ENN2", ENN2);
	("INC3", INC3); ("DEC3", DEC3); ("ENT3", ENT3); ("ENN3", ENN3);
	("INC4", INC4); ("DEC4", DEC4); ("ENT4", ENT4); ("ENN4", ENN4);
	("INC5", INC5); ("DEC5", DEC5); ("ENT5", ENT5); ("ENN5", ENN5);
	("INC6", INC6); ("DEC6", DEC6); ("ENT6", ENT6); ("ENN6", ENN6);
	("INCX", INCX); ("DECX", DECX); ("ENTX", ENTX); ("ENNX", ENNX);
	("CMPA", CMPA); ("FCMP", FCMP); ("CMP1", CMP1); ("CMP2", CMP2); ("CMP3", CMP3); ("CMP4", CMP4); ("CMP5", CMP5); ("CMP6", CMP6); ("CMPX", CMPX)]
;;

let opcode_table = Hashtbl.create (List.length opcode_list);;
let opcode_name_table = Hashtbl.create (List.length opcode_list);;

List.iter (fun (x,y) -> Hashtbl.add opcode_table x y) opcode_list ;;
List.iter (fun (x,y) -> Hashtbl.add opcode_name_table y x) opcode_list ;;

let string_of_opcode op =
	try Hashtbl.find opcode_name_table op with Not_found -> "<???>"

let opcode_of_string s = Hashtbl.find opcode_table s

let normal_field_setting op =
	match op with
	| NOP -> 0 | ADD -> 5 | FADD -> 6 | SUB -> 5 | FSUB -> 6 | MUL -> 5 | FMUL -> 6 | DIV -> 5 | FDIV -> 6 | NUM -> 0 | CHAR -> 1 | HLT -> 2
	| SLA -> 0 | SRA -> 1 | SLAX -> 2 | SRAX -> 3 | SLC -> 4 | SRC -> 5 | MOVE -> 1 | LDA -> 5 | LD1 -> 5
	| LD2 -> 5 | LD3 -> 5 | LD4 -> 5 | LD5 -> 5 | LD6 -> 5 | LDX -> 5 | LDAN -> 5 | LD1N -> 5 | LD2N -> 5
	| LD3N -> 5 | LD4N -> 5 | LD5N -> 5 | LD6N -> 5 | LDXN -> 5 | STA -> 5 | ST1 -> 5 | ST2 -> 5 | ST3 -> 5
	| ST4 -> 5 | ST5 -> 5 | ST6 -> 5 | STX -> 5 | STJ -> 2 | STZ -> 5 | JBUS -> 0 | IOC -> 0 | IN -> 0
	| OUT -> 0 | JRED -> 0 | JMP -> 0 | JSJ -> 1 | JOV -> 2 | JNOV -> 3 | JL -> 4 | JE -> 5 | JG -> 6
	| JGE -> 7 | JNE -> 8 | JLE -> 9
	| JAN -> 0 | JAZ -> 1 | JAP -> 2 | JANN -> 3 | JANZ -> 4 | JANP -> 5
	| J1N -> 0 | J1Z -> 1 | J1P -> 2 | J1NN -> 3 | J1NZ -> 4 | J1NP -> 5
	| J2N -> 0 | J2Z -> 1 | J2P -> 2 | J2NN -> 3 | J2NZ -> 4 | J2NP -> 5
	| J3N -> 0 | J3Z -> 1 | J3P -> 2 | J3NN -> 3 | J3NZ -> 4 | J3NP -> 5
	| J4N -> 0 | J4Z -> 1 | J4P -> 2 | J4NN -> 3 | J4NZ -> 4 | J4NP -> 5
	| J5N -> 0 | J5Z -> 1 | J5P -> 2 | J5NN -> 3 | J5NZ -> 4 | J5NP -> 5
	| J6N -> 0 | J6Z -> 1 | J6P -> 2 | J6NN -> 3 | J6NZ -> 4 | J6NP -> 5
	| JXN -> 0 | JXZ -> 1 | JXP -> 2 | JXNN -> 3 | JXNZ -> 4 | JXNP -> 5
	| INCA -> 0 | DECA -> 1 | ENTA -> 2 | ENNA -> 3
	| INC1 -> 0 | DEC1 -> 1 | ENT1 -> 2 | ENN1 -> 3
	| INC2 -> 0 | DEC2 -> 1 | ENT2 -> 2 | ENN2 -> 3
	| INC3 -> 0 | DEC3 -> 1 | ENT3 -> 2 | ENN3 -> 3
	| INC4 -> 0 | DEC4 -> 1 | ENT4 -> 2 | ENN4 -> 3
	| INC5 -> 0 | DEC5 -> 1 | ENT5 -> 2 | ENN5 -> 3
	| INC6 -> 0 | DEC6 -> 1 | ENT6 -> 2 | ENN6 -> 3
	| INCX -> 0 | DECX -> 1 | ENTX -> 2 | ENNX -> 3
	| CMPA -> 5 | FCMP -> 6 | CMP1 -> 5 | CMP2 -> 5 | CMP3 -> 5 | CMP4 -> 5 | CMP5 -> 5 | CMP6 -> 5 | CMPX -> 5

let opcode_number op =
	match op with
	| NOP -> 0 | ADD | FADD -> 1 | SUB | FSUB -> 2 | MUL | FMUL -> 3 | DIV | FDIV -> 4 | NUM | CHAR | HLT -> 5 | SLA | SLAX | SRA | SRAX | SLC | SRC -> 6
	| MOVE -> 7 | LDA -> 8 | LD1 -> 9 | LD2 -> 10 | LD3 -> 11 | LD4 -> 12 | LD5 -> 13 | LD6 -> 14 | LDX -> 15
	| LDAN -> 16 | LD1N -> 17 | LD2N -> 18 | LD3N -> 19 | LD4N -> 20 | LD5N -> 21 | LD6N -> 22 | LDXN -> 23
	| STA -> 24 | ST1 -> 25 | ST2 -> 26 | ST3 -> 27 | ST4 -> 28 | ST5 -> 29 | ST6 -> 30 | STX -> 31 | STJ -> 32 | STZ -> 33
	| JBUS -> 34 | IOC -> 35 | IN -> 36 | OUT -> 37 | JRED -> 38 | JMP | JSJ | JOV | JNOV | JL | JE | JG | JGE | JNE | JLE -> 39
	| JAN | JAZ | JAP | JANN | JANZ | JANP -> 40
	| J1N | J1Z | J1P | J1NN | J1NZ | J1NP -> 41
	| J2N | J2Z | J2P | J2NN | J2NZ | J2NP -> 42
	| J3N | J3Z | J3P | J3NN | J3NZ | J3NP -> 43
	| J4N | J4Z | J4P | J4NN | J4NZ | J4NP -> 44
	| J5N | J5Z | J5P | J5NN | J5NZ | J5NP -> 45
	| J6N | J6Z | J6P | J6NN | J6NZ | J6NP -> 46
	| JXN | JXZ | JXP | JXNN | JXNZ | JXNP -> 47
	| INCA | DECA | ENTA | ENNA -> 48
	| INC1 | DEC1 | ENT1 | ENN1 -> 49
	| INC2 | DEC2 | ENT2 | ENN2 -> 50
	| INC3 | DEC3 | ENT3 | ENN3 -> 51
	| INC4 | DEC4 | ENT4 | ENN4 -> 52
	| INC5 | DEC5 | ENT5 | ENN5 -> 53
	| INC6 | DEC6 | ENT6 | ENN6 -> 54
	| INCX | DECX | ENTX | ENNX -> 55
	| CMPA | FCMP -> 56 | CMP1 -> 57 | CMP2 -> 58 | CMP3 -> 59 | CMP4 -> 60 | CMP5 -> 61 | CMP6 -> 62 | CMPX -> 63

let opcode_of_number c f =
	match c, f with
	| 0, _ -> NOP | 1, 6 -> FADD | 1, _ -> ADD | 2, 6 -> FSUB | 2, _ -> SUB | 3, 6 -> FMUL | 3, _ -> MUL | 4, 6 -> FDIV | 4, _ -> DIV
	| 5, 0 -> NUM | 5, 1 -> CHAR | 5, 2 -> HLT | 6, 0 -> SLA | 6, 1 -> SRA | 6,2 -> SLAX | 6,3 -> SRAX | 6,4 -> SLC | 6,5 -> SRC
	| 7,_ -> MOVE | 8, _ -> LDA | 9, _ -> LD1 | 10, _ -> LD2 | 11, _ -> LD3 | 12, _ -> LD4 | 13, _ -> LD5 | 14, _ -> LD6 | 15, _ -> LDX
	| 16, _ -> LDAN | 17, _ -> LD1N | 18, _ -> LD2N | 19, _ -> LD3N | 20, _ -> LD4N | 21, _ -> LD5N | 22, _ -> LD6N | 23, _ -> LDXN
	| 24, _ -> STA | 25, _ -> ST1 | 26, _ -> ST2 | 27, _ -> ST3 | 28, _ -> ST4 | 29, _ -> ST5 | 30, _ -> ST6 | 31, _ -> STX
	| 32, _ -> STJ | 33, _ -> STZ | 34, _ -> JBUS | 35, _ -> IOC | 36, _ -> IN | 37, _ -> OUT | 38, _ -> JRED
	| 39, 0 -> JMP | 39, 1 -> JSJ | 39,2 -> JOV | 39, 3 -> JNOV | 39,4 -> JL | 39,5->JE | 39,6 -> JG | 39,7 -> JGE | 39,8 -> JNE | 39,9 -> JLE
	| 40,0 -> JAN | 40,1 -> JAZ | 40,2 -> JAP | 40,3 -> JANN | 40,4 -> JANZ | 40,5 -> JANP
	| 41,0 -> J1N | 41,1 -> J1Z | 41,2 -> J1P | 41,3 -> J1NN | 41,4 -> J1NZ | 41,5 -> J1NP
	| 42,0 -> J2N | 42,1 -> J2Z | 42,2 -> J2P | 42,3 -> J2NN | 42,4 -> J2NZ | 42,5 -> J2NP
	| 43,0 -> J3N | 43,1 -> J3Z | 43,2 -> J3P | 43,3 -> J3NN | 43,4 -> J3NZ | 43,5 -> J3NP
	| 44,0 -> J4N | 44,1 -> J4Z | 44,2 -> J4P | 44,3 -> J4NN | 44,4 -> J4NZ | 44,5 -> J4NP
	| 45,0 -> J5N | 45,1 -> J5Z | 45,2 -> J5P | 45,3 -> J5NN | 45,4 -> J5NZ | 45,5 -> J5NP
	| 46,0 -> J6N | 46,1 -> J6Z | 46,2 -> J6P | 46,3 -> J6NN | 46,4 -> J6NZ | 46,5 -> J6NP
	| 47,0 -> JXN | 47,1 -> JXZ | 47,2 -> JXP | 47,3 -> JXNN | 47,4 -> JXNZ | 47,5 -> JXNP
	| 48,0 -> INCA | 48,1 -> DECA | 48,2 -> ENTA | 48,3 -> ENNA
	| 49,0 -> INC1 | 49,1 -> DEC1 | 49,2 -> ENT1 | 49,3 -> ENN1
	| 50,0 -> INC2 | 50,1 -> DEC2 | 50,2 -> ENT2 | 50,3 -> ENN2
	| 51,0 -> INC3 | 51,1 -> DEC3 | 51,2 -> ENT3 | 51,3 -> ENN3
	| 52,0 -> INC4 | 52,1 -> DEC4 | 52,2 -> ENT4 | 52,3 -> ENN4
	| 53,0 -> INC5 | 53,1 -> DEC5 | 53,2 -> ENT5 | 53,3 -> ENN5
	| 54,0 -> INC6 | 54,1 -> DEC6 | 54,2 -> ENT6 | 54,3 -> ENN6
	| 55,0 -> INCX | 55,1 -> DECX | 55,2 -> ENTX | 55,3 -> ENNX
	| 56, 6 -> FCMP | 56,_ -> CMPA | 57,_ -> CMP1 | 58,_ -> CMP2 | 59,_ -> CMP3 | 60,_ -> CMP4 | 61,_ -> CMP5 | 62,_ -> CMP6 | 63,_ -> CMPX
	| _ -> failwith (Printf.sprintf "Unknown instruction C = %d F = %d.\n" c f)

type fieldt = Field | Unit

let field_kind op =
	match op with
	| ADD | SUB | MUL | DIV | LDA | LD1 | LD2 | LD3 | LD4 | LD5 | LD6 | LDX | LDAN | LD1N | LD2N | LD3N
	| LD4N | LD5N | LD6N | LDXN | STA | ST1 | ST2 | ST3 | ST4 | ST5 | ST6 | STX | STJ | STZ | CMPA | CMP1 | CMP2 | CMP3 | CMP4
	| CMP5 | CMP6 | CMPX -> Field
	| _ -> Unit
