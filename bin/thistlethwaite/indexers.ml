open ExtLib

open Utils_module.Utils
open Utils_module.Functions
open Utils_module.Types
open Rubiks_cube

let compute_edges_combinations (cube: rubiks_cube) edges edges_enums = 
  let nb_edges = Array.length edges in
  let edges_combos = Array.make 4 0 in
  let rec compute_edges_combinations_aux edge_index combo_index = 
    if edge_index >= nb_edges || combo_index >= 4 then ()
    else (
      let current_edge = edges.(edge_index) in
      let current_edge_index = cube#get_edge_index current_edge in 
      if (
        current_edge_index = int_of_edge_enum edges_enums.(0) || 
        current_edge_index = int_of_edge_enum edges_enums.(1) ||
        current_edge_index = int_of_edge_enum edges_enums.(2) ||
        current_edge_index = int_of_edge_enum edges_enums.(3)
      ) then (
        edges_combos.(combo_index) <- edge_index;
        compute_edges_combinations_aux (edge_index + 1) (combo_index + 1);
      )else (
        compute_edges_combinations_aux (edge_index + 1) combo_index;
      )
    ) in compute_edges_combinations_aux 0 0;
  edges_combos;;

let combinations_indexer comb n k = 
  let choises = Array.make_matrix n k 0 in 
  for i = 0 to n do
    for j = 0 to k do
      choises.(i).(j) <- combinations i j
    done
  done;
  let rank = ref choises.(n).(k) in
  Array.iter (fun comb -> 
      rank := !rank - choises.(n - (comb + 1)).(k - 1);
  ) comb;
  !rank - 1;;


let compute_tetrad_pairs (cube: rubiks_cube) corners_pair = 
  let corners = cube#get_corners () in
  let nb_corners = Array.length corners in

  let tetrad_pairs = Array.make 4 (Array.make 2 0) in 

  let rec compute_tetrad_pairs_aux (corners: corner array) index_pair = 
    if index_pair >= 4 then ()
    else (
      let c1, c2 = corners_pair.(index_pair) in
      let rec compute_tetrad_pair_aux_aux corner_index combo_index c1 c2 = 
        if corner_index >= nb_corners || combo_index >= 2 then ()
        else (
          let current_corner = corners.(corner_index) in
          let current_corner_index = cube#get_corner_index current_corner.c_enum in
          if (
            current_corner_index = int_of_corner_enum c1 || 
            current_corner_index = int_of_corner_enum c2
          ) then (
            tetrad_pairs.(index_pair).(combo_index) <- corner_index;
            compute_tetrad_pair_aux_aux (corner_index + 1) (combo_index + 1) c1 c2;
        ) else (
          compute_tetrad_pair_aux_aux (corner_index + 1) combo_index c1 c2;
        )
      ) in compute_tetrad_pair_aux_aux 0 0 c1 c2;
      compute_tetrad_pairs_aux corners (index_pair + 1);
    ) in compute_tetrad_pairs_aux corners 0 ;
  tetrad_pairs;;


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
    bases.(!i) <- bases.(!i + 1) * combinations ((n-2) - 2*(!i)) 2;
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

let compute_ones_lookup n = 
  let ones_lookup = Array.make (1 lsl n) 0 in
  for i = 0 to (1 lsl n) - 1 do
    let bits = BitSet.create i in
    ones_lookup.(i) <- BitSet.count bits;
  done;
  ones_lookup;;

let compute_factorials n k = 
  let factorials = Array.make n 0 in
  for i = 0 to n - 1 do
    factorials.(i) <- pick (n - 1 - i) (k - 1 - i);
  done;