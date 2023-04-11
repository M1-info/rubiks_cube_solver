open Types

let string_of_color color = 
  match color with
  | RED -> "R"
  | YELLOW -> "Y"
  | BLUE -> "B"
  | GREEN -> "G"
  | ORANGE -> "O"
  | WHITE -> "W";;

let color_from_int color = 
  match color with
  | 0 -> RED
  | 1 -> GREEN
  | 2 -> WHITE
  | 3 -> BLUE
  | 4 -> YELLOW
  | 5 -> ORANGE
  | _ -> failwith "Invalid color";;

let int_of_face face = 
  match face with
  | UP    ->  0
  | RIGHT ->  1
  | FRONT ->  2
  | LEFT  ->  3
  | BACK  ->  4
  | DOWN  ->  5;;

let face_from_int face = 
  match face with
  | 0 ->  UP
  | 1 ->  RIGHT
  | 2 ->  FRONT
  | 3 ->  LEFT
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


let get_corner_from_index index = 
  match index with
  | 0 -> URF 
  | 1 -> ULF 
  | 2 -> ULB 
  | 3 -> URB 
  | 4 -> DRF 
  | 5 -> DLF 
  | 6 -> DLB 
  | 7 -> DRB
  | _ -> failwith "Invalid corner index";;


let get_corner_from_enum enum =
  match enum with
  | URF -> 0
  | ULF -> 1
  | ULB -> 2
  | URB -> 3
  | DRF -> 4
  | DLF -> 5
  | DLB -> 6
  | DRB -> 7;;
  