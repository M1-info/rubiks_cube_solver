open ExtLib

open Utils_module.Utils
open Utils_module.Functions
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

let combinations_indexer combs n k = 
  let choises = Array.make_matrix (n + 1) (k + 1) 0 in 

  for i = 0 to n do
    for j = 0 to k do
      choises.(i).(j) <- combinations i j
    done
  done;
  
  let rank = ref choises.(n).(k) in
  for i = 0 to k - 1 do
    rank := !rank - choises.(n - (combs.(i) + 1)).(k - 1);
  done;
  !rank - 1;;


let compute_tetrad_pair (cube: rubiks_cube) (c1, c2) = 
  let corners = cube#get_corners () in
  let nb_corners = Array.length corners in
  let combo_index = ref 0 in
  let tetrad_pair = Array.make 2 0 in
  for i = 0 to nb_corners - 1 do 
    if !combo_index < 2 then (
      let corner_index = cube#get_corner_index (corner_enum_of_int i) in
      if (
        corner_index = int_of_corner_enum c1 || 
        corner_index = int_of_corner_enum c2
      ) then (
        tetrad_pair.(!combo_index) <- i;
        combo_index := !combo_index + 1;
      )
    );
  done;
  tetrad_pair;;


let generate_pairs n =
  let pairs = Array.make_matrix (n*(n-1)/2) 2 0 in 
  let pairs_index = ref 0 in
  let rec generate_pairs_aux i j = 
    if i >= n then ()
    else if j >= n then generate_pairs_aux (i + 1) (i + 2)
    else (
      pairs.(!pairs_index).(0) <- i;
      pairs.(!pairs_index).(1) <- j;
      pairs_index := !pairs_index + 1;
      generate_pairs_aux i (j + 1);
    ) in generate_pairs_aux 0 1;
  pairs;;
  
  
let generate_bases n = 
  let bases = Array.make ((n-2)/2) 0 in
  (bases.(((n-2)/2) - 1) <- 1);
  for i = ((n-2)/2) - 2 downto 0 do
    bases.(i) <- bases.(i+1) * combinations ((n - 2) - 2 * i) 2;
  done;
  bases;;
  
  
let pair_indexer tetrad_pairs nb_pairs =  
  let bases = generate_bases nb_pairs in
  let remaining = generate_pairs nb_pairs in 

  let rank = ref 0 in
  let num_remaining = ref (nb_pairs*(nb_pairs-1)/2) in
  for n = 0 to ((nb_pairs - 2) / 2) - 1 do
    let remaining_index = ref 0 in
    let pair = tetrad_pairs.(n) in

    for i = 0 to !num_remaining - 1 do
      let remaining_pair = remaining.(i) in
      if (remaining_pair.(0) = pair.(0) && remaining_pair.(1) = pair.(1)) then (
        rank := !rank + (i * bases.(n));
      ) else if (
        remaining_pair.(0) <> pair.(0) && remaining_pair.(0) <> pair.(1) && 
        remaining_pair.(1) <> pair.(0) && remaining_pair.(1) <> pair.(1)
      ) then (
        remaining.(!remaining_index).(0) <- remaining_pair.(0);
        remaining.(!remaining_index).(1) <- remaining_pair.(1);
        remaining_index := !remaining_index + 1;
      );
    done;
    num_remaining := !remaining_index;
  done;
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
  factorials;;


let permutations_indexer perm n k = 
  let factorials = compute_factorials n k in 
  let ones_lookup = compute_ones_lookup n in

  let lehmer = Array.make k 0 in 
  let bits_seen = BitSet.create n in

  (lehmer.(0) <- perm.(0);
  BitSet.set bits_seen (n - 1 - perm.(0)));

  let i = ref (-1) in 
  Array.fold_left (fun acc _ -> (
    i := !i + 1;
    BitSet.set bits_seen (n - 1 - perm.(!i));
    let ones = ones_lookup.((bitset_to_int bits_seen) lsr (n - perm.(!i))) in
    lehmer.(!i) <- perm.(!i) - ones;
    acc + lehmer.(!i) * factorials.(!i);
  )) 0 lehmer;;

