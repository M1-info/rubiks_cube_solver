open Types

let color_of_int = function
  | 0 -> RED
  | 1 -> BLUE
  | 2 -> WHITE
  | 3 -> GREEN
  | 4 -> YELLOW
  | 5 -> ORANGE
  | _ -> failwith "Invalid color";;


let int_of_face = function
  | UP    ->  0
  | LEFT  ->  1
  | FRONT ->  2
  | RIGHT ->  3
  | BACK  ->  4
  | DOWN  ->  5;;


let face_of_int = function
  | 0 ->  UP
  | 1 ->  LEFT
  | 2 ->  FRONT
  | 3 ->  RIGHT
  | 4 ->  BACK
  | 5 ->  DOWN
  | _ -> failwith "Invalid face";;
;;


let edge_enum_of_int = function
  | 0 ->  UB 
  | 1 ->  UR 
  | 2 ->  UF 
  | 3 ->  UL 
  | 4 ->  FR 
  | 5 ->  FL 
  | 6 ->  BL 
  | 7 ->  BR 
  | 8 ->  DF 
  | 9 ->  DL 
  | 10 -> DB
  | 11 -> DR
  | _ -> failwith "Invalid edge index";;


let int_of_edge_enum = function 
  | UB -> 0
  | UR -> 1
  | UF -> 2
  | UL -> 3
  | FR -> 4
  | FL -> 5
  | BL -> 6
  | BR -> 7
  | DF -> 8
  | DL -> 9
  | DB -> 10
  | DR -> 11;;


let corner_enum_of_int = function
  | 0 -> ULB 
  | 1 -> URB 
  | 2 -> URF 
  | 3 -> ULF 
  | 4 -> DLF 
  | 5 -> DLB 
  | 6 -> DRB 
  | 7 -> DRF
  | _ -> failwith "Invalid corner index";;


let int_of_corner_enum = function
  | ULB -> 0
  | URB -> 1
  | URF -> 2
  | ULF -> 3
  | DLF -> 4
  | DLB -> 5
  | DRB -> 6
  | DRF -> 7;;


let exponent_of_edge = function
  | UB -> 10
  | UR -> 9
  | UF -> 8
  | UL -> 7
  | FR -> 6
  | FL -> 5
  | BL -> 4
  | BR -> 3
  | DF -> 2
  | DL -> 1
  | DB -> 0
  | DR -> 0;;

let exponent_of_corner = function
  | ULB -> 6
  | URB -> 5
  | URF -> 4
  | ULF -> 3
  | DLF -> 2
  | DLB -> 1
  | DRB -> 0
  | DRF -> 0;;


let string_of_edge = function
  | UB -> "UB"
  | UR -> "UR"
  | UF -> "UF"
  | UL -> "UL"
  | FR -> "FR"
  | FL -> "FL"
  | BL -> "BL"
  | BR -> "BR"
  | DF -> "DF"
  | DL -> "DL"
  | DB -> "DB"
  | DR -> "DR";;


let string_of_corner = function 
  | URF -> "URF"
  | ULF -> "ULF"
  | ULB -> "ULB"
  | URB -> "URB"
  | DRF -> "DRF"
  | DLF -> "DLF"
  | DLB -> "DLB"
  | DRB -> "DRB";;


let string_of_move = function
  | U       -> "u"
  | U2      -> "u_2"
  | UPRIME  -> "u_prime"
  | D       -> "d"
  | D2      -> "d_2"
  | DPRIME  -> "d_prime"
  | L       -> "l"
  | L2      -> "l_2"
  | LPRIME  -> "l_prime"
  | R       -> "r"
  | R2      -> "r_2"
  | RPRIME  -> "r_prime"
  | F       -> "f"
  | F2      -> "f_2"
  | FPRIME  -> "f_prime"
  | B       -> "b"
  | B2      -> "b_2"
  | BPRIME  -> "b_prime"
  | None    -> ""
  | NoMove    -> "";;


let string_of_color = function 
  | RED ->    "R"
  | YELLOW -> "Y"
  | BLUE ->   "B"
  | GREEN ->  "G"
  | ORANGE -> "O"
  | WHITE ->  "W";;