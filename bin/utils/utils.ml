open Types

let string_of_color color = 
  match color with
  | RED ->    "R"
  | YELLOW -> "Y"
  | BLUE ->   "B"
  | GREEN ->  "G"
  | ORANGE -> "O"
  | WHITE ->  "W";;


let color_from_int color = 
  match color with
  | 0 -> RED
  | 1 -> BLUE
  | 2 -> WHITE
  | 3 -> GREEN
  | 4 -> YELLOW
  | 5 -> ORANGE
  | _ -> failwith "Invalid color";;


let int_of_face face = 
  match face with
  | UP    ->  0
  | LEFT  ->  1
  | FRONT ->  2
  | RIGHT ->  3
  | BACK  ->  4
  | DOWN  ->  5;;


let face_from_int face = 
  match face with
  | 0 ->  UP
  | 1 ->  LEFT
  | 2 ->  FRONT
  | 3 ->  RIGHT
  | 4 ->  BACK
  | 5 ->  DOWN
  | _ -> failwith "Invalid face";;
;;


let get_edge_from_index index = 
  match index with
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


let get_edge_from_enum enum = 
  match enum with
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


let get_edge_index edge = 
  get_edge_from_enum edge.edge;;


let get_edge_enum edge = 
  edge.edge;;


let get_corner_from_index index = 
  match index with
  | 0 -> ULB 
  | 1 -> URB 
  | 2 -> URF 
  | 3 -> ULF 
  | 4 -> DLF 
  | 5 -> DLB 
  | 6 -> DRB 
  | 7 -> DRF
  | _ -> failwith "Invalid corner index";;


let get_corner_from_enum enum =
  match enum with
  | ULB -> 0
  | URB -> 1
  | URF -> 2
  | ULF -> 3
  | DLF -> 4
  | DLB -> 5
  | DRB -> 6
  | DRF -> 7;;


let get_corner_index corner = 
  get_corner_from_enum corner.corner;;

let get_corner_enum corner = 
  corner.corner;;
  

let corner_to_string corner = 
  match corner with
  | URF -> "URF"
  | ULF -> "ULF"
  | ULB -> "ULB"
  | URB -> "URB"
  | DRF -> "DRF"
  | DLF -> "DLF"
  | DLB -> "DLB"
  | DRB -> "DRB";;


let move_to_string move = 
  match move with
  | U       -> "u"
  | U2      -> "u_2"
  | UPRIME  -> "u_prime'"
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
  | None    -> "";;
