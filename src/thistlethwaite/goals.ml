open Utils_module
open Rubiks_cube

(* 
   The first goal need to have all edges in a good orientation
   This mean that each edge can be permuted in their original position
   only using U, R, D, L moves. Theses mouvements can't flip edges.  
   In our case, we know that a edge is well orientend if its orientation is 0 (0 or 1 for edges)
*)
let goal_1 (cube: rubiks_cube) =   
  let rec is_satisfied_edges acc (edges: Types.edge list) = 
    match edges with
    | [] -> acc
    | edge::rest -> if edge.orientation = 1 then is_satisfied_edges false [] else is_satisfied_edges true rest 

  in is_satisfied_edges true (Array.to_list (cube#get_edges ()))
;;


  
(* The second goal need to have all corners in a good orientation.
   This mean that the corner have him left and right side well oriented in the left or right face.
   In our case, we know that a corner is well orientend if its orientation is 0 (0, 1 or 2 for corners) 

    We also need the side edges (FR, FL, BR, BL) to be in the right position (in the correct slice E, equator slice)
*)
let goal_2 (cube: rubiks_cube) = 
  let corners = Array.to_list (cube#get_corners ()) in

  let rec is_satisfied_corners acc (corners: Types.corner list) = 
    match corners with
    | [] -> acc
    | corner::rest -> if corner.orientation = 0 then (is_satisfied_corners true rest) 
                      else (is_satisfied_corners false [])
  in 

  let rec is_satisfied_edges acc (edges: Types.edge_enum list) = 
    match edges with
    | [] -> acc
    | edge::rest -> (
      (* Check if the current edge is in the range of the E-slice edges FR to BR *)
      if cube#get_edge_index edge >= Utils.int_of_edge_enum FR || cube#get_edge_index edge <= Utils.int_of_edge_enum BR
        then is_satisfied_edges true rest
        else is_satisfied_edges false []
    )
  in

  (is_satisfied_edges true [FR; FL; BL; BR] && is_satisfied_corners true corners)
;;


(*
  The third goal need to have all tetrad corners paired. {ULB, URF}, {DLF, DRB}...
  This make the corners solvable with only half twists.

  We also need to ensure that all edges are in their correct slice. 
  The E-slices are already positionned since the second goal is satisfied.
  So we only need to check if any M-slice edge is in a M-slice postion.

  We also need to ensure that the parity of the permutation corners is even.
*)
let goal_3 (cube: rubiks_cube) =

  let corners = cube#get_corners () in

  (* Check corners tetrad *)
  let pairs = [|(Types.ULB, Types.URF); (Types.URF, Types.ULB); (Types.DLF, Types.DRB); (Types.DRB, Types.DLF)|] in
  let pairs_length = Array.length pairs in
  let rec check_if_corners_are_paired acc i = 
    if i = pairs_length || acc = false then acc
    else (
      let c_1, c_2 = pairs.(i) in
      let index = cube#get_corner_index c_1 in 
      let is_paired = (index = Utils.int_of_corner_enum c_1 || index = Utils.int_of_corner_enum c_2) in
      if is_paired then check_if_corners_are_paired true (i + 1)
      else check_if_corners_are_paired false (i + 1)
    )
  in 
  let are_corners_paired = check_if_corners_are_paired true 0 in

  (* Check M-slices edges *)
  let edges = [| Types.UB; Types.UF; Types.DF; Types.DB |] in 
  let edges_length = Array.length edges in
  let rec check_is_edges_are_in_M_slice acc i = 
    if i = edges_length || acc = false then acc
    else (
      let edge = edges.(i) in 
      let index = cube#get_edge_index edge in
      let bool = (index = Utils.int_of_edge_enum edges.(0) || 
                  index = Utils.int_of_edge_enum edges.(1) || 
                  index = Utils.int_of_edge_enum edges.(2) || 
                  index = Utils.int_of_edge_enum edges.(3)
                ) 
      in 
      if bool then check_is_edges_are_in_M_slice true (i + 1)
      else check_is_edges_are_in_M_slice false (i + 1)
    )
  in 
  let are_edges_in_M_slice = check_is_edges_are_in_M_slice true 0 in

  (* Check parity of corners permutation *)

  let nb_corners = Array.length corners in
  let rec get_corners_parity acc i j =
    if i >= nb_corners && j >= nb_corners then acc
    else (
      if j = nb_corners then get_corners_parity acc (i + 1) (i + 2) 
      else (
        let bool = if (corners.(i).orientation < corners.(j).orientation) then 1 else 0 in
        get_corners_parity (acc lxor bool) i (j + 1)
      )
    )
  in 
  let is_parity_even = (get_corners_parity 0 0 1) = 0 in

  (are_corners_paired && are_edges_in_M_slice && is_parity_even)
;;
    
(*
  The fourth goal is to solve the cube. 
  At this point we only do half twists (180°), 
  We only need to ensure that all edges are in their correct slice
  and that all corners are in their correct slice.  
*)
let goal_4 (cube: rubiks_cube) =
  let edges = cube#get_edges () in
  let corners = cube#get_corners () in

  (* 
    We already got parity of the permutation corners in the previous goals.
    So we know that at this point two edges can't be swapped, 
    we only can work with 10 edges and not 12.  
  *)
  let nb_edges = (Array.length edges) - 2 in

  let rec is_satisfied_edges acc (edges: Types.edge list) i = 
    match edges with
    | [] -> acc
    | edge::rest -> if i = nb_edges then acc
                    else if  edge.e_index <> i then is_satisfied_edges false [] 0 
                    else is_satisfied_edges acc rest (i + 1)
  in 

  (* 
    We already know that the tetrads corners are paired.
    It is not possible to have 3 oriented corners out of position, 
    so we only need to check 5 corners.
  *)
  let nb_corners = (Array.length corners) - 3 in
  let rec is_satisfied_corners acc (corners: Types.corner list) i = 
    match corners with
    | [] -> acc
    | corner::rest -> if i = nb_corners then acc
                      else if  corner.c_index <> i then is_satisfied_corners false [] 0 
                      else is_satisfied_corners acc rest (i + 1)
  in 
  
  (is_satisfied_edges true (Array.to_list edges) 0 && is_satisfied_corners true (Array.to_list corners) 0)
;; 
  

let get_goal = function 
  | Types.Goal_1 -> goal_1
  | Types.Goal_2 -> goal_2
  | Types.Goal_3 -> goal_3
  | Types.Goal_4 -> goal_4
;;