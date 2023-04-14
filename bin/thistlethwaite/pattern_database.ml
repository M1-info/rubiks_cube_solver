open Utils_module.Types
open Utils_module.Functions
open Rubiks_cube
open Indexers

class pattern_database = 
  object (self)
    val mutable group_1 = []
    val mutable group_2 = []
    val mutable group_3 = []
    val mutable group_4 = []

  method init =
    self#load_group_1;
    self#load_group_2;
    self#load_group_3;
    self#load_group_4;

  method load_group_1 = group_1 <- load_file "bin/databases/thistlethwiateG1.pdb";

  method load_group_2 = group_2 <- load_file "bin/databases/thistlethwiateG2.pdb";

  method load_group_3 = group_3 <- load_file "bin/databases/thistlethwiateG3.pdb";
  
  method load_group_4 = group_4 <- load_file "bin/databases/thistlethwiateG4.pdb";

  
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
    BR = 16
    BL = 8
    DF = 4
    DL = 2
    DB = 0
  *)
  method get_index_group_1 (cube: rubiks_cube) = 
    let edges = (cube#get_edges ()) in
    let nb_edges = Array.length edges in
    let index = Array.fold_left (fun acc edge -> 
      if edge.e_enum = DR then acc 
      else (
        let edge_index = cube#get_edge_index edge.e_enum in 
        let exponent = Int.abs ((nb_edges - 1) - edge_index) in
        acc + ((pow 2 exponent) * edge.orientation);
      )
    ) 0 edges in 
    index;

  
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
    let corners = cube#get_corners () in
    let nb_corners = Array.length corners in
    let edges_map = Array.map (fun edge -> edge.e_enum) (cube#get_edges ()) in
    let rank = combinations_indexer (compute_edges_combinations cube edges_map [| FR; FL; BR; BL |]) 12 4 in

    let orientation_corners_num = Array.fold_left ( fun acc corner ->
      if corner.c_enum = DRF then acc 
      else (
        let index = cube#get_corner_index corner.c_enum in 
        let exponent = Int.abs ((nb_corners - 1) - index) in
        acc + ((pow 3 exponent) * corner.orientation);
      )
    ) 0 corners in

    (* 2187 = 3^7 (7 are the corners and 3 because they are indexed in base 3) *)
    let index = (rank * 2187) + orientation_corners_num in
    index;


  method get_index_group_3 (cube: rubiks_cube) =
    let pairs = [| ULB , URF ; DLF , DRB ; URB , ULF ; DLB , DRF |] in

    let corners_pairs = compute_tetrad_pairs cube pairs in 

    let corners_rank = pair_indexer corners_pairs in 

    let edges_map = [| UB; UR; UF; UL; DF; DL; DB; DR |] in 

    let edges_rank = combinations_indexer (compute_edges_combinations cube edges_map [| UB; UF; DF; DB |]) 12 4 in
    
    let parity = check_corners_parity (cube#get_corners ()) in

    ((edges_rank * 2520 + corners_rank) * 2 + !parity);


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

end;;
