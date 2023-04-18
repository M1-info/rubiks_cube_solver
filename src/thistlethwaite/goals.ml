open Stdint

open Utils_module.Types
open Utils_module.Utils
open Utils_module.Functions
open Rubiks_cube

let goal_0_to_1 (cube: rubiks_cube) =

  (* 
     The first goal need to have all edges in a good orientation
     This mean that each edge can be permuted in their original position
     only using U, R, D, L moves. Theses mouvements are called can't flip edges.  
     In our case, we know that a edge is well orientend if its orientation is 0 (0 or 1 for edges)
  *)
     
  let rec is_satisfied_edges acc (edges: edge list) = 
    match edges with
    | [] -> acc
    | edge::rest -> if edge.orientation = Uint8.one then is_satisfied_edges false [] else is_satisfied_edges true rest 
  in is_satisfied_edges true (Array.to_list (cube#get_edges ()));;


  
let goal_1_to_2 (cube: rubiks_cube) = 

  (* The second goal need to have all corners in a good orientation.
     This mean that the corner have him left and right side well oriented in the left or right face.
     In our case, we know that a corner is well orientend if its orientation is 0 (0, 1 or 2 for corners) 

      We also need the side edges (FR, FL, BR, BL) to be in the right position (in the correct slice E, equator slice)
  *)

  let corners = Array.to_list (cube#get_corners ()) in

  let rec is_satisfied_corners acc (corners: corner list) = 
    match corners with
    | [] -> acc
    | corner::rest -> if corner.orientation <> Uint8.zero then (is_satisfied_corners false []) 
                      else (is_satisfied_corners true rest)
  in 
  if (
    (* Check if any E-slice edge is in a E-slice postion *)
    (cube#get_edge_index FR >= 4 || cube#get_edge_index FR <= 7) && 
    (cube#get_edge_index FL >= 4 || cube#get_edge_index FL <= 7) &&
    (cube#get_edge_index BL >= 4 || cube#get_edge_index BL <= 7) &&
    (cube#get_edge_index BR >= 4 || cube#get_edge_index BR <= 7) &&
    (* Check if all corners are oriented *)
    is_satisfied_corners true corners
  ) then true else false;;


let goal_2_to_3 (cube: rubiks_cube) =

  let corners = cube#get_corners () in
  let pair_index = ref 0 in

  pair_index := cube#get_corner_index ULB;
  let first_pair = !pair_index = int_of_corner_enum ULB || !pair_index = int_of_corner_enum URF in

  pair_index := cube#get_corner_index URF;
  let first_pair_inverse = !pair_index = int_of_corner_enum URF || !pair_index = int_of_corner_enum ULB in

  pair_index := cube#get_corner_index DLF;
  let second_pair = !pair_index = int_of_corner_enum DLF || !pair_index = int_of_corner_enum DRB in

  pair_index := cube#get_corner_index DRB;
  let second_pair_inverse = !pair_index = int_of_corner_enum DRB || !pair_index = int_of_corner_enum DLF in

  (* let index_urb = cube#get_corner_index URB in
  let third_pair = index_urb = int_of_corner_enum URB || index_urb = int_of_corner_enum ULF in
  let index_ulf = cube#get_corner_index ULF in
  let third_pair_inverse = index_ulf = int_of_corner_enum ULF || index_ulf = int_of_corner_enum URB in *)

  let parity = get_corners_parity corners in

  print_newline ();

  print_string "Parity: ";
  print_string (string_of_bool (parity = 0));
  print_newline ();

  print_string "First pair: ";
  print_string (string_of_bool first_pair);
  print_newline ();

  print_string "First pair inverse: ";
  print_string (string_of_bool first_pair_inverse);
  print_newline ();

  print_string "Second pair: ";
  print_string (string_of_bool second_pair);
  print_newline ();

  print_string "Second pair inverse: ";
  print_string (string_of_bool second_pair_inverse);
  print_newline ();

  print_string "UB: ";
  print_string (string_of_bool (cube#get_edge_index UB = 0 || cube#get_edge_index UB = 2 || cube#get_edge_index UB = 8 || cube#get_edge_index UB = 10));
  print_newline ();

  print_string "UF: ";
  print_string (string_of_bool (cube#get_edge_index UF = 0 || cube#get_edge_index UF = 2 || cube#get_edge_index UF = 8 || cube#get_edge_index UF = 10));
  print_newline ();
  
  print_string "DF: ";
  print_string (string_of_bool (cube#get_edge_index DF = 0 || cube#get_edge_index DF = 2 || cube#get_edge_index DF = 8 || cube#get_edge_index DF = 10));
  print_newline ();

  print_string "DB: ";
  print_string (string_of_bool (cube#get_edge_index DB = 0 || cube#get_edge_index DB = 2 || cube#get_edge_index DB = 8 || cube#get_edge_index DB = 10));
  print_newline ();

  print_string "------------------------";
  print_newline ();

  if (
    first_pair && second_pair && first_pair_inverse && second_pair_inverse && (* third_pair && third_pair_inverse && *)
    (* Check if any M-slice edge is in a M-slice postion *)
    (cube#get_edge_index UB = 0 || cube#get_edge_index UB = 2 || cube#get_edge_index UB = 8 || cube#get_edge_index UB = 10) && 
    (cube#get_edge_index UF = 0 || cube#get_edge_index UF = 2 || cube#get_edge_index UF = 8 || cube#get_edge_index UF = 10) &&
    (cube#get_edge_index DF = 0 || cube#get_edge_index DF = 2 || cube#get_edge_index DF = 8 || cube#get_edge_index DF = 10) &&
    (cube#get_edge_index DB = 0 || cube#get_edge_index DB = 2 || cube#get_edge_index DB = 8 || cube#get_edge_index DB = 10) &&
    parity = 0 
  ) then true else false;;
    

let goal_3_to_4 (cube: rubiks_cube) =
  let edges = cube#get_edges () in
  let corners = cube#get_corners () in

  (* Explain why we loop over only 10 edges *)
  let nb_edges = (Array.length edges) - 2 in

  let rec is_satisfied_edges acc edges i = 
    match edges with
    | [] -> acc
    | edge::rest -> if i = nb_edges then acc
                    else if (int_of_edge_enum edge.e_enum) <> i then is_satisfied_edges false [] 0 
                    else is_satisfied_edges acc rest (i + 1)
  in 

  (* Explain why we loop over only 10 edges *)
  let nb_corners = (Array.length corners) - 3 in
  let rec is_satisfied_corners acc corners i = 
    match corners with
    | [] -> acc
    | corner::rest -> if i = nb_corners then acc
                      else if (int_of_corner_enum corner.c_enum) <> i then is_satisfied_corners false [] 0 
                      else is_satisfied_corners acc rest (i + 1)
  in 
  is_satisfied_edges true (Array.to_list edges) 0 && is_satisfied_corners true (Array.to_list corners) 0;; 
  

let get_goal = function 
  | 0 -> goal_0_to_1
  | 1 -> goal_1_to_2
  | 2 -> goal_2_to_3
  | 3 -> goal_3_to_4
  | _ -> failwith "Goal not found";;