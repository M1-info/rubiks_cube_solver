open Utils_module.Types
open Utils_module.Utils
(* open Rubiks_cube *)

let goal_0_to_1 edges =
  let rec is_satisfied acc edges = 
    match edges with
    | [] -> acc
    | edge::rest -> if edge.orientation <> 0 then is_satisfied false [] else is_satisfied acc rest 
  in is_satisfied true edges;;
  
let goal_1_to_2 cube corners = 
  let rec is_satisfied_corners acc corners = 
    match corners with
    | [] -> acc
    | corner::rest -> if corner.orientation <> 0 then is_satisfied_corners false [] else is_satisfied_corners acc rest 
  in 
  if (
    cube#find_edge_index FR > 3 && 
    cube#find_edge_index FL > 3 &&
    cube#find_edge_index BR > 3 &&
    cube#find_edge_index BL > 3 && 
    is_satisfied_corners true corners
  ) then true else false;;


(* let goal_2_to_3 cube corners =
  let index = cube#find_corner_index ULB in
  let first_pair = not (index <> get_corner_from_enum ULB && index <> get_corner_from_enum URF) in
  let index = cube#find_corner_index URF in
  let second_pair = not (index <> get_corner_from_enum URF  && index <> get_corner_from_enum ULB) in
  let index = cube#find_corner_index DLF in
  let third_pair = not (index <> get_corner_from_enum DLF && index <> get_corner_from_enum DRB) in
  let index = cube#find_corner_index DRB in
  let fourth_pair = not (index <> get_corner_from_enum DRB && index <> get_corner_from_enum DRF) in
  let nb_corners = (Array.length corners) - 1 in
  let rec check_parity parity i j = 
    if i = nb_corners + 1 then parity
    else (
      parity <- parity lxor (corners.(i).orientation < corners.(j).orientation);
      if j = nb_corners then check_parity parity (i + 1) 0
      else check_parity parity i (j + 1)
    ) 
  in 
  if (
    first_pair && second_pair && third_pair && fourth_pair &&
    cube#find_edge_index UB > 3 && 
    cube#find_edge_index UF > 3 &&
    cube#find_edge_index DB > 3 &&
    cube#find_edge_index DF > 3
  ) then true else false;; *)
    
