open Stdint

open Utils_module.Types
open Utils_module.Functions
open Utils_module.Utils
open Rubiks_cube
open Indexers

class pattern_database = 
  object (self)
    val mutable group_1 = {size = 0; data = [||]}
    val mutable group_2 = {size = 0; data = [||]}
    val mutable group_3 = {size = 0; data = [||]}
    val mutable group_4 = {size = 0; data = [||]}

  method init () =
    (* print_newline (); *)
    (* Printf.printf "Loading pattern databases...\n"; *)
    (* Printf.printf "Loading group 1 : \n"; *)
    self#load_group_1;
    (* Printf.printf "Group 1 loaded !\n"; *)
    (* print_newline (); *)

    (* Printf.printf "Loading group 2 : \n"; *)
    self#load_group_2;

    (* Printf.printf "Group 2 loaded !\n"; *)
    (* print_newline (); *)
    (* Printf.printf "Loading group 3 : \n"; *)
    self#load_group_3;

    (* Printf.printf "Group 3 loaded !\n"; *)
    (* print_newline (); *)
    (* Printf.printf "Loading group 4 : \n"; *)
    (* self#load_group_4; *)
    (* Printf.printf "Pattern databases loaded !\n"; *)
    (* print_newline (); *)

  method get_group_1 () = group_1;
  method get_group_2 () = group_2;
  method get_group_3 () = group_3;
  method get_group_4 () = group_4;

  method load_group_1 = group_1 <- load_file "src/databases/thistlethwiateG1.pdb";
  method load_group_2 = group_2 <- load_file "src/databases/thistlethwiateG2.pdb";
  method load_group_3 = group_3 <- load_file "src/databases/thistlethwiateG3.pdb";
  method load_group_4 = group_4 <- load_file "src/databases/thistlethwiateG4.pdb";

  method get_group = function
    | 0 -> self#get_group_1 ()
    | 1 -> self#get_group_2 ()
    | 2 -> self#get_group_3 ()
    | 3 -> self#get_group_4 ()
    | _ -> failwith "Invalid group number"

  method get_index group cube = 
    match group with 
    | 0 -> self#get_index_group_1 cube
    | 1 -> self#get_index_group_2 cube
    | 2 -> self#get_index_group_3 cube
    | 3 -> self#get_index_group_4 cube
    | _ -> failwith "Invalid group number"

  (*
    With a cube state, we compute the index of the pattern database of the group 1
    For the group 1 we need to solve the edges of the cube.
    We use the original position of each edge in the solved cube and his orientation
    We only need to compute 11 edges because with 11 edges we forcely dictate the position of the last one.
    
    We use the formula :
    index = sum (2^i * orientation) with i the index of the edge in the array multiplied by the base 2 power
    
    UB = 1024
    UR = 512
    UF = 256
    UL = 128
    FR = 64
    FL = 32
    BL = 16
    BR = 8
    DF = 4
    DL = 2
    DB = 0
  *)
  method get_index_group_1 (cube: rubiks_cube) = 
    let edges = (cube#get_edges ()) in
    Array.fold_left (fun acc edge -> 
      if edge.e_enum = DR then acc 
      else (
        let orientation = cube#get_edge_orientation edge.e_enum in
        let exponent = exponent_of_edge edge.e_enum in
        acc + ((pow 2 exponent) * (Uint8.to_int orientation));
      )
    ) 0 edges;

  
  (*
    With a cube state, we compute the index of the pattern database of the group 2
    For the group 2 we need to solve the corners of the cube.
    We use the original position of each corner in the solved cube and his orientation
    We only need to compute 7 corners because with 7 corners we forcely dictate the position of the last one.
    
    We use the formula :
    index = sum (3^i * orientation) with i the index of the corner in the array multiplied by the base 3 power
    
    DRF = 2187
    DLF = 729
    URB = 243
    ULF = 81
    DRB = 27
    URB = 9
    ULB = 3
    DBL = 0    
  *)
  method get_index_group_2 (cube: rubiks_cube) = 
    let edges = cube#get_edges () in

    let edges_length = Array.length edges in

    let edge_combinations = Array.make 4 0 in 
    let combinations_index = ref 0 in 
    for i = 0 to edges_length - 1 do 
      if !combinations_index < 4 then (
        let edge_index = cube#get_edge_index (edge_enum_of_int i) in
        if (
          edge_index = int_of_edge_enum FR ||
          edge_index = int_of_edge_enum FL ||
          edge_index = int_of_edge_enum BL ||
          edge_index = int_of_edge_enum BR
        ) then (
          edge_combinations.(!combinations_index) <- i;
          combinations_index := !combinations_index + 1;
        )
      );
    done;

    let rank = combinations_indexer edge_combinations 12 4 in

    (* let corner_orientations = [|
      ULB, cube#get_corner_orientation ULB;
      ULB, cube#get_corner_orientation URB;
      ULB, cube#get_corner_orientation URF;
      ULB, cube#get_corner_orientation ULF;
      ULB, cube#get_corner_orientation DLF;
      ULB, cube#get_corner_orientation DLB;
      ULB, cube#get_corner_orientation DRB;
    |] in *)

    let corner_orientations = [|
      cube#get_corner_orientation ULB;
      cube#get_corner_orientation URB;
      cube#get_corner_orientation URF;
      cube#get_corner_orientation ULF;
      cube#get_corner_orientation DLF;
      cube#get_corner_orientation DLB;
      cube#get_corner_orientation DRB;
    |] in

    let orientation_corners_num = 
      Uint8.to_int corner_orientations.(0) * 729 +
      Uint8.to_int corner_orientations.(1) * 243 +
      Uint8.to_int corner_orientations.(2) * 81 +
      Uint8.to_int corner_orientations.(3) * 27 +
      Uint8.to_int corner_orientations.(4) * 9 +
      Uint8.to_int corner_orientations.(5) * 3 +
      Uint8.to_int corner_orientations.(6)
    in

    (* let orientation_corners_num = Array.fold_left ( fun acc (corner, orientation) ->
      let exponent = exponent_of_corner corner in
      acc + ((pow 3 exponent) * Uint8.to_int orientation);
    ) 0 corner_orientations in *)

    (* 2187 = 3^7 (7 are the corners and 3 because they are indexed in base 3) *)
    let index = (rank * 2187) + orientation_corners_num in
    index;


  method get_index_group_3 (cube: rubiks_cube) =
    let pairs = [| (ULB , URF) ; (DLF , DRB) ; (URB , ULF) ; (DLB , DRF) |] in
    let corners_pair = Array.make 4 (Array.make 2 0) in 

    Array.iteri (fun index _ -> 
      corners_pair.(index) <- compute_tetrad_pair cube pairs.(index);
    ) pairs;

    let corners_rank = pair_indexer corners_pair 8 in 

    let all_edges = cube#get_edges () in
    let edges_length = Array.length all_edges in

    let edges_map = [|0;1;2;3;0;0;0;0;4;5;6;7|] in

    let edge_combinations = Array.make 4 0 in 
    let edge_combinations_index = ref 0 in

    for i = 0 to (edges_length - 1) do
      if(!edge_combinations_index < 4) then (
        let edge_index = cube#get_edge_index (edge_enum_of_int i) in
        if  (
              edge_index = int_of_edge_enum UB || 
              edge_index = int_of_edge_enum UF ||
              edge_index = int_of_edge_enum DF ||
              edge_index = int_of_edge_enum DB
            ) 
            then (
              edge_combinations.(!edge_combinations_index) <- edges_map.(i);
              edge_combinations_index := !edge_combinations_index + 1;  
            )
      );
    done;
    
    let edges_rank = combinations_indexer edge_combinations 8 4 in

    let parity = get_corners_parity (cube#get_corners ()) in

    (* 2520 -> 8C2*6C2*4C2 *)
    (edges_rank * 2520 + corners_rank) * 2 + parity;


  method get_index_group_4 (cube: rubiks_cube) = 

    let edges_map = Array.make 12 0 in

    ((* M slice edges *)
    edges_map.(cube#get_edge_index UB) <- 0;
    edges_map.(cube#get_edge_index UF) <- 1;
    edges_map.(cube#get_edge_index DF) <- 2;
    edges_map.(cube#get_edge_index DB) <- 3;

    (* S slice edges *)
    edges_map.(cube#get_edge_index UR) <- 0;
    edges_map.(cube#get_edge_index UL) <- 1;
    edges_map.(cube#get_edge_index DL) <- 2;
    edges_map.(cube#get_edge_index DR) <- 3;

    (* E slice edges *)
    edges_map.(cube#get_edge_index FR) <- 0;
    edges_map.(cube#get_edge_index FL) <- 1;
    edges_map.(cube#get_edge_index BL) <- 2;
    edges_map.(cube#get_edge_index BR) <- 3);

    let m_edges = [| 
      edges_map.(cube#get_edge_index UB);
      edges_map.(cube#get_edge_index UF);
      edges_map.(cube#get_edge_index DF);
      edges_map.(cube#get_edge_index DB);
    |] in

    let s_edges = [| 
      edges_map.(cube#get_edge_index UR);
      edges_map.(cube#get_edge_index UL);
      edges_map.(cube#get_edge_index DL);
      edges_map.(cube#get_edge_index DR);
    |] in

    let e_edges = [|
      edges_map.(cube#get_edge_index FR);
      edges_map.(cube#get_edge_index FL);
    |] in

    let m_rank = permutations_indexer m_edges 4 4 in 
    let s_rank = permutations_indexer s_edges 4 4 in
    let e_rank = permutations_indexer e_edges 4 2 in

    let edges_rank = m_rank * 288 + s_rank * 12 + e_rank in 

    let corner_map = Array.make 8 0 in 

    (corner_map.(cube#get_corner_index ULB) <- 0;
    corner_map.(cube#get_corner_index URF) <- 1;
    corner_map.(cube#get_corner_index DLF) <- 2;
    corner_map.(cube#get_corner_index DRB) <- 3;

    corner_map.(cube#get_corner_index URB) <- 0;
    corner_map.(cube#get_corner_index ULF) <- 1;
    corner_map.(cube#get_corner_index DLB) <- 2;
    corner_map.(cube#get_corner_index DRF) <- 3);

    let tetrad_pair = [|
      corner_map.(cube#get_corner_index ULB);
      corner_map.(cube#get_corner_index URF);
      corner_map.(cube#get_corner_index DLF);
      corner_map.(cube#get_corner_index DRB);
    |] in

    let corner = corner_map.(cube#get_corner_index URB) in
    let tetrad_rank = permutations_indexer tetrad_pair 4 4 in
    let corner_rank = tetrad_rank * 4 + corner in

    (* 96 = 4!*4 *)
    edges_rank * 96 + corner_rank;

  method get_num_moves index group = 
    let i = index / 2 in
    let value = group.data.(i) in

    if index mod 2 <> 0 then (int_of_char value) land 0x0F
    else ((int_of_char value) lsr 4);


  method set_num_moves group index num_moves = 
    let old_num_moves = self#get_num_moves index group in

    if old_num_moves = 0xF then (
      group.size <- group.size + 1;
    );

    if (old_num_moves > num_moves) then (
      let i = index / 2 in
      let current_value = int_of_char group.data.(i) in
      if index mod 2 <> 0 then (
        let new_value = (current_value land 0xF0) lor (num_moves land 0x0F) in
        group.data.(i) <- char_of_int new_value;
      )
      else (
        let new_value = (num_moves lsl 4) lor (current_value land 0x0F) in
        group.data.(i) <- char_of_int new_value;
      );
      true;
    )else(
      false;
    )

end;;
