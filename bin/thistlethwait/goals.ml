open Utils_module.Types
open Utils_module.Utils
open Utils_module.Functions

let goal_0_to_1 edges =

  (* 
     The first goal need to have all edges in a good orientation
     This mean that each edge can be permuted in their original position
     only using U, R, D, L moves. Theses mouvements are called can't flip edges.  
     In our case, we know that a edge is well orientend if its orientation is 0 (0 or 1 for edges)
  *)

  let rec is_satisfied acc edges = 
    match edges with
    | [] -> acc
    | edge::rest -> if edge.orientation <> 0 then is_satisfied false [] else is_satisfied acc rest 
  in is_satisfied true (Array.to_list edges);;


  
let goal_1_to_2 edges corners = 

  (* The second goal need to have all corners in a good orientation.
     This mean that the corner have him left and right side well oriented in the left or right face.
     In our case, we know that a corner is well orientend if its orientation is 0 (0, 1 or 2 for corners) 

      We also need the side edges (FR, FL, BR, BL) to be in the right position (in the correct slice E, equator slice)
  *)

  let rec is_satisfied_corners acc corners = 
    match corners with
    | [] -> acc
    | corner::rest -> if corner.orientation <> 0 then is_satisfied_corners false [] else is_satisfied_corners acc rest 
  in 
  if (
    find_edge_index edges FR > 3 && 
    find_edge_index edges FL > 3 &&
    find_edge_index edges BR > 3 &&
    find_edge_index edges BL > 3 && 
    is_satisfied_corners true (Array.to_list corners)
  ) then true else false;;


let goal_2_to_3 edges corners =

  (*  *)

  let index = find_corner_index corners ULB in
  let first_pair = not (index <> get_corner_from_enum ULB && index <> get_corner_from_enum URF) in
  let index = find_corner_index corners URF in
  let first_pair_inverse = not (index <> get_corner_from_enum URF  && index <> get_corner_from_enum ULB) in

  let index = find_corner_index corners DLF in
  let second_pair = not (index <> get_corner_from_enum DLF && index <> get_corner_from_enum DRB) in
  let index = find_corner_index corners DRB in
  let second_pair_inverse = not (index <> get_corner_from_enum DRB && index <> get_corner_from_enum DRF) in

  let index = find_corner_index corners URB in
  let third_pair = not (index <> get_corner_from_enum URB && index <> get_corner_from_enum ULF) in
  let index = find_corner_index corners ULF in
  let third_pair_inverse = not (index <> get_corner_from_enum ULF && index <> get_corner_from_enum URB) in

  let nb_corners = Array.length corners in
  let rec check_parity parity i j = 
    if i = nb_corners then parity
    else (
      let result = if (corners.(i).orientation < corners.(j).orientation) then 1 else 0 in
      parity := !parity lxor result;
      if j = nb_corners - 1 then check_parity parity (i + 1) 0
      else check_parity parity i (j + 1)
    ) in 
  let is_parity_even = check_parity (ref 0) 0 1 = ref 0 in

  if (
    first_pair && second_pair && first_pair_inverse && second_pair_inverse && third_pair && third_pair_inverse &&
    find_edge_index edges UB > 3 && 
    find_edge_index edges UF > 3 &&
    find_edge_index edges DB > 3 &&
    find_edge_index edges DF > 3 && 
    is_parity_even
  ) then true else false;;
    

let goal_3_to_4 edges corners =
  (* Explain why we loop over only 10 edges *)
  let nb_edges = (Array.length edges) - 2 in

  let rec is_satisfied_edges acc edges i = 
    match edges with
    | [] -> acc
    | item::rest -> if i = nb_edges then acc
                      else if (get_edge_from_enum item.edge) <> i then is_satisfied_edges false [] 0 
                      else is_satisfied_edges acc rest (i + 1)
  in 

  (* Explain why we loop over only 10 edges *)
  let nb_corners = (Array.length corners) - 3 in
  let rec is_satisfied_corners acc corners i = 
    match corners with
    | [] -> acc
    | item::rest -> if i = nb_corners then acc
                      else if (get_corner_from_enum item.corner) <> i then is_satisfied_corners false [] 0 
                      else is_satisfied_corners acc rest (i + 1)
  in 
  is_satisfied_edges true (Array.to_list edges) 0 && is_satisfied_corners true (Array.to_list corners) 0;; 
   