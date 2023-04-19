open Utils_module
open Rubiks_cube

class pattern_database = 
  object (self)

    val mutable group_0_to_1: Types.database = {size = 0; data = [||]}
    val mutable group_1_to_2: Types.database = {size = 0; data = [||]}
    val mutable group_2_to_3: Types.database = {size = 0; data = [||]}
    val mutable group_3_to_4: Types.database = {size = 0; data = [||]}


    method init () =
      self#load_group_0_to_1;
      self#load_group_1_to_2;
      self#load_group_2_to_3;
      self#load_group_3_to_4;


    (* Getters *)
    method get_group_0_to_1 () = group_0_to_1;
    method get_group_1_to_2 () = group_1_to_2;
    method get_group_2_to_3 () = group_2_to_3;
    method get_group_3_to_4 () = group_3_to_4;


    (* Pattern databases loaders *)
    method load_group_0_to_1 = group_0_to_1 <- Functions.load_file "src/databases/thistlethwiateG1.pdb";
    method load_group_1_to_2 = group_1_to_2 <- Functions.load_file "src/databases/thistlethwiateG2.pdb";
    method load_group_2_to_3 = group_2_to_3 <- Functions.load_file "src/databases/thistlethwiateG3.pdb";
    method load_group_3_to_4 = group_3_to_4 <- Functions.load_file "src/databases/thistlethwiateG4.pdb";


    (* Pattern databases getter *)
    method get_group = function
      | Types.Group_0_1 -> self#get_group_0_to_1 ()
      | Types.Group_1_2 -> self#get_group_1_to_2 ()
      | Types.Group_2_3 -> self#get_group_2_to_3 ()
      | Types.Group_3_4 -> self#get_group_3_to_4 ()


    (* Index of a cube state getter *)
    method get_index group_index cube = 
      match group_index with 
      | Types.Group_0_1 -> self#get_index_group_0_to_1 cube
      | Types.Group_1_2 -> self#get_index_group_1_to_2 cube
      | Types.Group_2_3 -> self#get_index_group_2_to_3 cube
      | Types.Group_3_4 -> self#get_index_group_3_to_4 cube


    (*
      With a cube state, we compute the index of the pattern database of the group 1
      For the group 1 we need each edge to be in the right orientation.

      Each edge can be in 2 different orientations. So we use the base 2 to compute the index.
      We only need to compute 11 edges because with 11 edges we forcely dictate the position of the last one.

      In this database are stored : 2^11 = 1024 values

      We use the formula :
      index = sum (2^i * orientation) with i the index of the edge in the array multiplied by the base 2 power
      
      Exponents are stores in the Utils module, in the exponent_of_edge function.
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
      DR (not used, with 11 edges we forcely got the orientatoin of the last one)
    *)
    method get_index_group_0_to_1 (cube: rubiks_cube) = 
      let edges = (cube#get_edges ()) in
      Array.fold_left (fun acc (edge: Types.edge) -> 
        if edge.e_index = Utils.int_of_edge_enum DR then acc 
        else (
          let orientation = cube#get_edge_orientation (Utils.edge_enum_of_int edge.e_index) in
          let exponent = Utils.exponent_of_edge (Utils.edge_enum_of_int edge.e_index) in
          acc + ((Functions.pow 2 exponent) * orientation);
        )
      ) 0 edges;

  
    (*
      With a cube state, we compute the index of the pattern database of the group 2
      For the group 2 we need to solve the corners of the cube and the E-slices edges.

      For the edges we compute the combination of the 4 edges in the E-slices.
      That give us the index of the edge combination in the pattern database.

      For the corners we compute the orientation of each corner multiplied by the base 3 power.
      Each corner can be in 3 different orientations. So we use the base 3 to compute the index.


      In this database are stored : 12C4 * 3^7 = 1082565 values

      We use the formula :
      index = sum (3^i * orientation) with i the index of the corner in the array multiplied by the base 3 power
      
      Exponents are stores in the Utils module, in the exponent_of_corner function.
      ULB = 729
      URR = 243
      URF = 81
      ULF = 27
      DLF = 9
      DLB = 3
      DRB = 0    
      DRF (not used, with 7 corners we forcely got the orientatoin of the last one)
    *)
    method get_index_group_1_to_2 (cube: rubiks_cube) = 
      let edges = cube#get_edges () in

      let get_edge_combinations cube edges =
        let edges_length = Array.length edges in
        let rec loop_combinations comb comb_index i =
          if i = edges_length - 1 || comb_index >= 4 then comb
          else
            let edge_index = cube#get_edge_index (Utils.edge_enum_of_int i) in
            if (
              edge_index = Utils.int_of_edge_enum Types.FR ||
              edge_index = Utils.int_of_edge_enum Types.FL ||
              edge_index = Utils.int_of_edge_enum Types.BL ||
              edge_index = Utils.int_of_edge_enum Types.BR
            ) then
              (comb.(comb_index) <- i;
              loop_combinations comb (comb_index + 1) (i + 1))
            else
              loop_combinations comb comb_index (i + 1)
        in
        loop_combinations (Array.make 4 0) 0 0
      in

      let edge_combinations = get_edge_combinations cube edges in
      let rank = Int.abs (Indexers.combinations_indexer edge_combinations 12 4) in

      let corner_orientations = [|
        (Types.ULB , cube#get_corner_orientation Types.ULB);
        (Types.URB , cube#get_corner_orientation Types.URB);
        (Types.URF , cube#get_corner_orientation Types.URF);
        (Types.ULF , cube#get_corner_orientation Types.ULF);
        (Types.DLF , cube#get_corner_orientation Types.DLF);
        (Types.DLB , cube#get_corner_orientation Types.DLB);
        (Types.DRB , cube#get_corner_orientation Types.DRB);
      |] in

      let orientation_corners_num = Array.fold_left (fun acc (enum, orientation) -> 
        acc + ((Functions.pow 3 (Utils.exponent_of_corner enum)) * orientation)
      ) 0 corner_orientations in

      (* 2187 = 3^7 (7 are the corners and 3 because they are indexed in base 3) *)
      ((rank * 2187) + orientation_corners_num);


    (*
      Get the index of a given cube in the group 3 pattern database.
      
      We use the tetrad-paired corners to get the index. 
      With 8 corners we can get 4 tetrad-paired corners combinations.
      For the corners we have 8C2*6C2*4C2 combinations (2520 at all)
      
      For the edges we already know the position of the 4 edges in the E-slices.
      So for 8 edges we have 8C4 combinations (70 at all)

      Finally, we store the paity of corners and edges. This double the size of the pattern database.
      The last bit of the index is the parity of the corners and edges.

      In the database are stored : 70 * 2520 * 2 = 352800 values

      So, for get the index we use the formula :
      70 * 2520 * 2 + parity.
    *)
    method get_index_group_2_to_3 (cube: rubiks_cube) =
      (* Tetrad pairs *)
      let pairs = [| 
        (Types.ULB , Types.URF) ; 
        (Types.DLF , Types.DRB) ; 
        (Types.URB , Types.ULF) ; 
        (Types.DLB , Types.DRF) 
      |] in
      let corners_pair = Array.make 4 (Array.make 2 0) in 

      Array.iteri (fun index _ -> 
        corners_pair.(index) <- Indexers.compute_tetrad_pair cube pairs.(index);
      ) pairs;

      let corners_rank = Indexers.pair_indexer corners_pair 8 in 

      (* Edges slices *)
      let all_edges = cube#get_edges () in
      let edges_length = Array.length all_edges in

      let edges_map = Array.make edges_length 0 in
      edges_map.(Utils.int_of_edge_enum Types.UB) <- 0;
      edges_map.(Utils.int_of_edge_enum Types.UR) <- 1;
      edges_map.(Utils.int_of_edge_enum Types.UF) <- 2;
      edges_map.(Utils.int_of_edge_enum Types.UL) <- 3;
      edges_map.(Utils.int_of_edge_enum Types.FR) <- 3;
      edges_map.(Utils.int_of_edge_enum Types.FL) <- 6;
      edges_map.(Utils.int_of_edge_enum Types.BL) <- 4;
      edges_map.(Utils.int_of_edge_enum Types.BR) <- 5;
      edges_map.(Utils.int_of_edge_enum Types.DF) <- 4;
      edges_map.(Utils.int_of_edge_enum Types.DL) <- 5;
      edges_map.(Utils.int_of_edge_enum Types.DB) <- 6;
      edges_map.(Utils.int_of_edge_enum Types.DR) <- 7;

      let rec get_edges_combinations comb comb_index i =
        if i = edges_length || comb_index >= 4 then comb
        else (
          let edge_index = cube#get_edge_index (Utils.edge_enum_of_int i) in
          if  (
            edge_index = Utils.int_of_edge_enum Types.UB || 
            edge_index = Utils.int_of_edge_enum Types.UF ||
            edge_index = Utils.int_of_edge_enum Types.DF ||
            edge_index = Utils.int_of_edge_enum Types.DB
          ) 
          then (
            comb.(comb_index) <- edges_map.(i);
            get_edges_combinations comb (comb_index + 1) (i + 1);
          ) else (
            get_edges_combinations comb comb_index (i + 1);
          );
        )
      in 
      let edges_combinations = get_edges_combinations (Array.make 4 0) 0 0 in
      let edges_rank = Indexers.combinations_indexer edges_combinations 8 4 in

      (* Parity *)
      let nb_corners = Array.length (cube#get_corners ()) in
      let rec get_corners_parity acc i j = 
        if i >= nb_corners && j >= nb_corners then acc
        else (
          if j = nb_corners then get_corners_parity acc (i + 1) (i + 2)
          else (
            let bool = if (cube#get_corner_index (Utils.corner_enum_of_int i)) < (cube#get_corner_index (Utils.corner_enum_of_int j)) 
                       then 1 else 0
            in get_corners_parity (acc lxor bool) i (j + 1);
          )
        )
      in
      let parity = get_corners_parity 0 0 1 in

      (* 2520 -> 8C2*6C2*4C2 *)
      (edges_rank * 2520 + corners_rank) * 2 + parity;


    (*
      At this state we have the 4 E-slices edges in the correct slice and we fixed the parity in the previous group.
      This pattern store the permutations of the M-slice and S-slice edges (and 2 E-slice edges).
      We give un : 4! * 4! * 4C2 (order don't matters) = 6912 values

      We also got parity for corners in the previous group, and with half of twists (180°)
      is not possible for 3 corners to be out of place.
      That mean that 4!*4!/(2*3) = 96 corner permutations are reachable.

      In total this database store : 6912 * 96 = 663552 values

      To get the index in the database we use the formula :
      - edge_rank [0, 6911]
      - corner_rank [0, 95]
      edges_rank * 96 + corner_rank
    *)
    method get_index_group_3_to_4 (cube: rubiks_cube) = 

      let edges_map = Array.make 12 0 in

      (* M slice edges *)
      edges_map.(Utils.int_of_edge_enum Types.UB) <- 0;
      edges_map.(Utils.int_of_edge_enum Types.UF) <- 1;
      edges_map.(Utils.int_of_edge_enum Types.DF) <- 2;
      edges_map.(Utils.int_of_edge_enum Types.DB) <- 3;

      (* S slice edges *)
      edges_map.(Utils.int_of_edge_enum Types.UR) <- 0;
      edges_map.(Utils.int_of_edge_enum Types.UL) <- 1;
      edges_map.(Utils.int_of_edge_enum Types.DL) <- 2;
      edges_map.(Utils.int_of_edge_enum Types.DR) <- 3;

      (* E slice edges *)
      edges_map.(Utils.int_of_edge_enum Types.FR) <- 0;
      edges_map.(Utils.int_of_edge_enum Types.FL) <- 1;
      edges_map.(Utils.int_of_edge_enum Types.BL) <- 2;
      edges_map.(Utils.int_of_edge_enum Types.BR) <- 3;

      let m_edges = [| 
        edges_map.(cube#get_edge_index Types.UB);
        edges_map.(cube#get_edge_index Types.UF);
        edges_map.(cube#get_edge_index Types.DF);
        edges_map.(cube#get_edge_index Types.DB);
      |] in

      let s_edges = [| 
        edges_map.(cube#get_edge_index Types.UR);
        edges_map.(cube#get_edge_index Types.UL);
        edges_map.(cube#get_edge_index Types.DL);
        edges_map.(cube#get_edge_index Types.DR);
      |] in

      let e_edges = [|
        edges_map.(cube#get_edge_index Types.FR);
        edges_map.(cube#get_edge_index Types.FL);
      |] in

      let m_rank = Indexers.permutations_indexer m_edges 4 4 in 
      let s_rank = Indexers.permutations_indexer s_edges 4 4 in
      let e_rank = Indexers.permutations_indexer e_edges 4 2 in

      (* 288 = 4! * 4C2 (without order) / 12 = 4C2 *)
      let edges_rank = m_rank * 288 + s_rank * 12 + e_rank in 

      let corner_map = Array.make 8 0 in 

      corner_map.(Utils.int_of_corner_enum Types.ULB) <- 0;
      corner_map.(Utils.int_of_corner_enum Types.URF) <- 1;
      corner_map.(Utils.int_of_corner_enum Types.DLF) <- 2;
      corner_map.(Utils.int_of_corner_enum Types.DRB) <- 3;

      corner_map.(Utils.int_of_corner_enum Types.URB) <- 0;
      corner_map.(Utils.int_of_corner_enum Types.ULF) <- 1;
      corner_map.(Utils.int_of_corner_enum Types.DLB) <- 2;
      corner_map.(Utils.int_of_corner_enum Types.DRF) <- 3;

      let tetrad_pair = [|
        corner_map.(cube#get_corner_index Types.ULB);
        corner_map.(cube#get_corner_index Types.URF);
        corner_map.(cube#get_corner_index Types.DLF);
        corner_map.(cube#get_corner_index Types.DRB);
      |] in

      (* Random choise of one of the second tetrad pair *)
      let corner = corner_map.(cube#get_corner_index Types.URB) in
      let tetrad_rank = Indexers.permutations_indexer tetrad_pair 4 4 in
      let corner_rank = tetrad_rank * 4 + corner in

      (* 96 = 4!*4 *)
      edges_rank * 96 + corner_rank;


    (* Get the value of a index in a specific pattern database *)
    method get_num_moves (index: int) (group: Types.database) = 
      let i = index / 2 in
      let value = group.data.(i) in

      (* Data is stored in 1 bytes, and we need to get the MBS or the LBS depends if the index is odd or even *)
      if index mod 2 <> 0 then (int_of_char value) land 0x0F
      else ((int_of_char value) lsr 4);


    (* Set the value of a index in a specific pattern database *)
    method set_num_moves (index: int) (group: Types.database) (num_moves: int) = 
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
