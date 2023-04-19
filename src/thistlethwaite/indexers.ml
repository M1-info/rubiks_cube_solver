open ExtLib
open Utils_module
open Utils_module.Utils
open Rubiks_cube

(* This module contains functions that are used to index the various permutations of the Rubik's cube. *)


(* Compute nCk combinations *)
let combinations_indexer combs n k = 
  let rec compute_choises choises i j = 
    if i = n + 1 then choises
    else (
      choises.(i).(j) <- Functions.combinations i j;
      if j = k then compute_choises choises (i + 1) 0
      else compute_choises choises i (j + 1);
    )
  in 
  let choises = compute_choises (Array.make_matrix (n + 1) (k + 1) 0) 0 0 in

  let rec compute_rank rank i =
    if i = k then rank 
    else (
      compute_rank (rank - choises.(n - (combs.(i) + 1)).(k - i)) (i + 1);
    ) 
  in 
  let rank = compute_rank choises.(n).(k) 0 in
  rank - 1
;;


(* Compute the positions of a corners pair in the cube *)
let compute_tetrad_pair (cube: rubiks_cube) (c1, c2) = 
  let corners = cube#get_corners () in
  let nb_corners = Array.length corners in

  let rec compute_tetrad_pair_aux tetrad_pair combo_index i = 
    if i = nb_corners || combo_index >= 2 then tetrad_pair
    else (
      let corner_index = cube#get_corner_index (corner_enum_of_int i) in

      if (corner_index = int_of_corner_enum c1 || corner_index = int_of_corner_enum c2) then (
        tetrad_pair.(combo_index) <- i;
        compute_tetrad_pair_aux tetrad_pair (combo_index + 1) (i + 1);
      ) else (
        compute_tetrad_pair_aux tetrad_pair combo_index (i + 1);
      )
    )
  in compute_tetrad_pair_aux (Array.make 2 0) 0 0
;;


(* 
  Compute all unique pair in the range [0...N] in lexicographic order
  {0 1}, {0 2}, ..., {0 7}
  {1 2}, {1 3}, ..., {1 7}
  ...
  {6 7}
*)
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
  pairs
;;

  
(* 
  Compute the bases for ranked pairs 
  From the last base to the first :
  (2C2),( 4C2 * 2C2), (6C2 * 4C2 * 2C2), ...
*)
let generate_bases n = 
  let bases = Array.make ((n-2)/2) 0 in
  (bases.(((n-2)/2) - 1) <- 1);
  let rec generate_bases_aux i = 
    if i < 0 then bases
    else (
      bases.(i) <- bases.(i + 1) * Functions.combinations ((n - 2) - 2 * i) 2;
      generate_bases_aux (i - 1);
    ) 
  in generate_bases_aux (((n-2)/2) - 2)
;;

  
(*
  Iterate over all the pairs in the range [0...N]
  and then for each pair, iterate over all the remaining pairs in the range [0...N]
  to compute the rank of the pair.

  Each time we find a pair that is not in the remaining pairs, we add it to the remaining pairs.
  else we increase the rank by the corresponding base.
*)
let pair_indexer tetrad_pairs nb_pairs =  
  let bases = generate_bases nb_pairs in
  let remaining = generate_pairs nb_pairs in 

  let rec compute_rank rank nb_remaining i =
    if i = ((nb_pairs - 2) / 2) then rank
    else (
      let rec compute_rank_aux rank pair nb_remaining remaining_index j = 
        if j = nb_remaining then (rank, remaining_index)
        else (
          let remaining_pair = remaining.(j) in
          if (remaining_pair.(0) = pair.(0) && remaining_pair.(1) = pair.(1)) then (
            (* If the pair is the same so we increase the rank by the corresponding base *)
            compute_rank_aux (rank + (j * bases.(i))) pair nb_remaining remaining_index (j + 1)
          ) else if (
            remaining_pair.(0) <> pair.(0) && remaining_pair.(0) <> pair.(1) && 
            remaining_pair.(1) <> pair.(0) && remaining_pair.(1) <> pair.(1)
          ) then (
            (* If the pair is not the same and the remaining pair is not in the pair *)
            (* We need to add the remaining pair to the remaining pairs *)
            (* We also need to increase the number of remaining pairs *)
            remaining.(remaining_index).(0) <- remaining_pair.(0);
            remaining.(remaining_index).(1) <- remaining_pair.(1);
            compute_rank_aux rank pair nb_remaining (remaining_index + 1) (j + 1)
          ) else (
            compute_rank_aux rank pair nb_remaining remaining_index (j + 1)
          )
        )
      in
      let pair = tetrad_pairs.(i) in
      let remaining_index = 0 in
      let (new_rank, new_nb_remaining) = compute_rank_aux rank pair nb_remaining remaining_index 0 in 
      compute_rank new_rank new_nb_remaining (i + 1);
    ) in compute_rank 0 (nb_pairs*(nb_pairs-1)/2) 0


(*
  Generate array with the number of ones in the binary representation
  from a range [0...N]
*)
let compute_ones_lookup n = 
  let size = 1 lsl n in
  let ones_lookup = Array.make (size - 1) 0 in
  let rec compute_ones_lookup_aux i = 
    if i = size - 2 then ()
    else (
      let bits = Functions.bitarray (size-1) i in
      ones_lookup.(i) <- BitSet.count bits;
      compute_ones_lookup_aux (i + 1);
    ) in compute_ones_lookup_aux 0;
  ones_lookup;;

(*
  Generate array with the factorials from a range [0...N]
  Each value is the nCk result without order matters
*)
let compute_factorials n k = 
  let factorials = Array.make k 0 in
  Array.iteri (fun i _ -> factorials.(i) <- Functions.pick (n - 1 - i) (k - 1 - i)) factorials;
  factorials;;

(*
  Compute the rank of a permutation in lexicographic order
*)
let permutations_indexer perm n k = 
  let factorials = compute_factorials n k in 
  let ones_lookup = compute_ones_lookup n in

  let lehmer = Array.make k 0 in 
  let bits_seen = BitSet.create n in

  (* the first element of a lehmer code is always the first digit *)
  lehmer.(0) <- perm.(0);

  (* Bits seen in a permutation *)
  BitSet.set bits_seen (n - 1 - perm.(0));

  (*
    We convert the lehmer code to the base-10 representation
    We use the lehmer code to compute the rank of the permutation
    
    The lehmer code is the computed in a factorial number system
  *)
  let rec permutations_indexer_aux rank i = 
    if i = k then rank
    else (
      if (i >= 1) then (
        BitSet.set bits_seen (n - 1 - perm.(i));
        let ones = ones_lookup.((Functions.bitset_to_int bits_seen) lsr (n - perm.(i))) in
        lehmer.(i) <- perm.(i) - ones;
      );
      permutations_indexer_aux (rank + (lehmer.(i) * factorials.(i))) (i + 1);
    )
  in permutations_indexer_aux 0 0
;;

