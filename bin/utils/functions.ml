open Types
open Utils

let find_edge_index edges edge_enum = 
  let rec find_edge_index_helper edge_enum index = 
    if index >= Array.length edges then failwith "Value not found in array"
    else if edges.(index).edge = edge_enum then index
    else find_edge_index_helper edge_enum (index + 1)
  in
  find_edge_index_helper edge_enum 0;;


let find_edge_index_from_edge edges edge = 
  find_edge_index edges edge.edge;;


let find_corner_index corners corner_enum = 
  let rec find_corner_index_helper corner_enum index = 
    if index >= Array.length corners then failwith "Value not found in array"
    else if corners.(index).corner = corner_enum then index
    else find_corner_index_helper corner_enum (index + 1)
  in
  find_corner_index_helper corner_enum 0;;


let find_corner_index_from_corner corners corner =
  find_corner_index corners corner.corner;;


let compute_edge_combo edges enums = 
  let nb_edges = Array.length edges in
  let edges_combos = Array.make 4 0 in
  let rec compute_edge_combo_aux edge_index combo_index = 
    if edge_index >= nb_edges || combo_index >= 4 then ()
    else (
      let current_edge_index = find_edge_index_from_edge edges edges.(edge_index) in 
      if (
        current_edge_index = get_edge_from_enum enums.(0) || 
        current_edge_index = get_edge_from_enum enums.(1) ||
        current_edge_index = get_edge_from_enum enums.(2) ||
        current_edge_index = get_edge_from_enum enums.(3)
      ) then (
        edges_combos.(combo_index) <- edge_index;
        compute_edge_combo_aux (edge_index + 1) (combo_index + 1);
      )else (
        compute_edge_combo_aux (edge_index + 1) combo_index;
      )
    ) in compute_edge_combo_aux 0 0;
  edges_combos;;


let get_tetrad_pairs corners corners_pair = 
  let nb_corners = Array.length corners in

  let tetrad_pairs = Array.make 4 (Array.make 2 0) in 

  let rec compute_tetrad_pairs corners index_pair = 
    if index_pair >= 4 then ()
    else (
      let c1, c2 = corners_pair.(index_pair) in
      let rec compute_tetrad_pair_aux corner_index combo_index c1 c2 = 
        if corner_index >= nb_corners || combo_index >= 2 then ()
        else (
          let current_corner_index = find_corner_index_from_corner corners corners.(corner_index) in
          if (
            current_corner_index = get_corner_from_enum c1 || 
            current_corner_index = get_corner_from_enum c2
          ) then (
            tetrad_pairs.(index_pair).(combo_index) <- corner_index;
            compute_tetrad_pair_aux (corner_index + 1) (combo_index + 1) c1 c2;
        ) else (
          compute_tetrad_pair_aux (corner_index + 1) combo_index c1 c2;
        )
      ) in compute_tetrad_pair_aux 0 0 c1 c2;
      compute_tetrad_pairs corners (index_pair + 1);
    ) in compute_tetrad_pairs corners 0 ;
  tetrad_pairs;;


let check_corners_parity corners = 
  let nb_corners = Array.length corners in
  let rec check_corners_parity_aux parity i j = 
    if i = nb_corners then parity
    else (
      let result = if (corners.(i).orientation < corners.(j).orientation) then 1 else 0 in
      parity := !parity lxor result;
      if j = nb_corners - 1 then check_corners_parity_aux parity (i + 1) 0
      else check_corners_parity_aux parity i (j + 1)
    ) in
  let parity = check_corners_parity_aux (ref 0) 0 0 in
  parity;;


let pow a b = 
  let rec pow_helper a b acc = 
    if b = 0 then acc
    else pow_helper a (b - 1) (acc * a)
  in
  pow_helper a b 1;;


let factorial n =
  let rec factorial_rt n acc = 
    if n < 0 then failwith "n must be positiv"
    else if 0 = n then acc
    else factorial_rt (n-1) (n * acc)
  in factorial_rt n 1 ;;


let combinaison n k = 
  if n < k then 0
  else (factorial n) / (factorial (n - k) * (factorial k));;


let combinaison_indexer combinaisons n k = 
  let choises = Array.make_matrix n k 0 in 
  for i = 0 to n do
    for j = 0 to k do
      choises.(i).(j) <- combinaison i j
    done
  done;
  let rank = ref choises.(n).(k) in
  Array.iter (fun comb -> 
      rank := !rank - choises.(n - (comb + 1)).(k - 1);
  ) combinaisons;
  !rank - 1;;


let generate_pairs n =
  let pairs = Array.make (n*(n-1)/2) (Array.make 2 0) in 
  let rec generate_pair_aux pair_index pair pairs_index = 
    if pair_index == 2 then (
      pairs.(pairs_index) <- pair;
    ) else (
      let start = if pair_index = 0 then 0 else pair.(pair_index - 1) + 1 in
      let i = ref start in
      while !i > n - 1 do
        pair.(pair_index) <- !i;
        generate_pair_aux (pair_index + 1) pair (pairs_index + 1);
        i := !i + 1;
      done; 
    ) in generate_pair_aux 0 pairs.(0) 0;
  pairs;;


let generate_bases n = 
  let bases = Array.make ((n-2)/2) 0 in
  (bases.(((n-2)/2) - 1) <- 1);
  let i = ref (((n-2)/2) - 2) in
  while !i >= 0 do
    bases.(!i) <- bases.(!i + 1) * combinaison ((n-2) - 2*(!i)) 2;
    i := !i - 1;
  done;
  bases;;


let pair_indexer tetrad_pairs = 
  let generated_pairs = generate_pairs 8 in 
  let bases = generate_bases 8 in
  let rank = ref 0 in
  let num_remaining = ref (8*(8-1)/2) in
  let remaining = ref generated_pairs in 
  Array.iter ( fun pair -> 
    let remaining_index = ref 0 in
    for j = 0 to !num_remaining do
      let remaining_pair = !remaining.(j) in
      if (remaining_pair.(0) = pair.(0) && remaining_pair.(1) = pair.(1)) then (
        rank := j * bases.(!num_remaining - 1);
      ) else if (
        remaining_pair.(0) <> pair.(0) && remaining_pair.(1) <> pair.(1) && 
        remaining_pair.(0) <> pair.(1) && remaining_pair.(1) <> pair.(0)
      ) then !remaining.(!remaining_index) <- remaining_pair;
    done;
    num_remaining := !remaining_index;
  ) tetrad_pairs;
  !rank;;


