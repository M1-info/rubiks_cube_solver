open Utils_module.Utils
open Utils_module.Functions
(* open Stdint *)
(* open Rubiks_cube *)

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
    
  method load_file filename =
    let data = ref [] in
    let chan = open_in_bin filename in
    try
      while true do
        data := input_line chan :: !data
      done;
      assert false with End_of_file ->
        close_in chan;
        List.rev !data;

  method load_group_1 =
    group_1 <- self#load_file "bin/databases/thistlethwiateG1.pdb";

  method load_group_2 =
    group_2 <- self#load_file "bin/databases/thistlethwiateG2.pdb";

  method load_group_3 =
    group_3 <- self#load_file "bin/databases/thistlethwiateG3.pdb";
  
  method load_group_4 =
    group_4 <- self#load_file "bin/databases/thistlethwiateG4.pdb";

  
  (*
    We compute the index of the edge in the group 1
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
  method get_index_group_1 edges = 
    let nb_edges = Array.length edges in
    let index = Array.fold_left (fun acc edge -> 
      if get_edge_enum edge = DR then acc 
      else (
        let index = get_edge_index edge in 
        let exponent = Int.abs ((nb_edges - 1) - index) in
        acc + ((pow 2 exponent) * edge.orientation);
      )
    ) 0 edges in 
    index;

  
  method get_index_group_2 edges corners = 
    let nb_edges = Array.length edges in
    let nb_corners = Array.length corners in

    let compute_edge_combo () = 
      let edges_combos = Array.make 4 0 in
      let rec compute_edge_combo_aux edge_index combo_index = 
        if edge_index >= nb_edges || combo_index >= 4 then ()
        else (
          let edge_index = find_edge_index_from_edge edges edges.(edge_index) in 
          if (
            edge_index = get_edge_from_enum FR || 
            edge_index = get_edge_from_enum FL ||
            edge_index = get_edge_from_enum BR ||
            edge_index = get_edge_from_enum BL
          ) then (
            edges_combos.(combo_index) <- edge_index;
            compute_edge_combo_aux (edge_index + 1) (combo_index + 1);
          )else (
            compute_edge_combo_aux (edge_index + 1) combo_index;
          )
        ) in compute_edge_combo_aux 0 0;
      edges_combos;
    in
    let rank = combinaison_indexer (compute_edge_combo ()) (12) (4) in

    let orientation_corners_num = Array.fold_left ( fun acc corner ->
      if get_corner_enum corner = DRF then acc 
      else (
        let index = get_corner_index corner in 
        let exponent = Int.abs ((nb_corners - 1) - index) in
        acc + ((pow 3 exponent) * corner.orientation);
      )
    ) 0 corners in

    let index = (rank * 2187) + orientation_corners_num in
    index;
  end;;
