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

    let is_satisfied = ref true in
    let edges = Array.to_list (cube#get_edges ()) in
    for i = 0 to (List.length edges) - 1 do
      let edge = List.nth edges i in
      if edge.orientation <> 0 then is_satisfied := false
    done;
    !is_satisfied;;

  (* let edges = Array.to_list (cube#get_edges ()) in

  let rec is_satisfied_edges acc (edges: edge list) = 
    match edges with
    | [] -> acc
    | edge::rest -> if edge.orientation = 1 then is_satisfied_edges false [] else is_satisfied_edges acc rest 
  in is_satisfied_edges true edges;; *)


  
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
    | corner::rest -> if corner.orientation <> 0 then is_satisfied_corners false [] else is_satisfied_corners acc rest 
  in 
  if (
    cube#get_edge_index FR > 3 && 
    cube#get_edge_index FL > 3 &&
    cube#get_edge_index BR > 3 &&
    cube#get_edge_index BL > 3 && 
    is_satisfied_corners true corners
  ) then true else false;;


let goal_2_to_3 (cube: rubiks_cube) =

  let corners = cube#get_corners () in

  let index = cube#get_corner_index ULB in
  let first_pair = not (index <> int_of_corner_enum ULB && index <> int_of_corner_enum URF) in
  let index = cube#get_corner_index URF in
  let first_pair_inverse = not (index <> int_of_corner_enum URF  && index <> int_of_corner_enum ULB) in

  let index = cube#get_corner_index DLF in
  let second_pair = not (index <> int_of_corner_enum DLF && index <> int_of_corner_enum DRB) in
  let index = cube#get_corner_index DRB in
  let second_pair_inverse = not (index <> int_of_corner_enum DRB && index <> int_of_corner_enum DRF) in

  let index = cube#get_corner_index URB in
  let third_pair = not (index <> int_of_corner_enum URB && index <> int_of_corner_enum ULF) in
  let index = cube#get_corner_index ULF in
  let third_pair_inverse = not (index <> int_of_corner_enum ULF && index <> int_of_corner_enum URB) in

  let is_parity_even = check_corners_parity corners = ref 0 in

  if (
    first_pair && second_pair && first_pair_inverse && second_pair_inverse && third_pair && third_pair_inverse &&
    cube#get_edge_index UB > 3 && 
    cube#get_edge_index UF > 3 &&
    cube#get_edge_index DB > 3 &&
    cube#get_edge_index DF > 3 && 
    is_parity_even
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