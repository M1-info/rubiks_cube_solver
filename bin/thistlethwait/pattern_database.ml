open Utils_module.Utils
open Utils_module.Functions
open Utils_module.Types

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
    let nb_corners = Array.length corners in

    let rank = combinaison_indexer (compute_edge_combo edges [| FR; FL; BR; BL |]) 12 4 in

    let orientation_corners_num = Array.fold_left ( fun acc corner ->
      if get_corner_enum corner = DRF then acc 
      else (
        let index = get_corner_index corner in 
        let exponent = Int.abs ((nb_corners - 1) - index) in
        acc + ((pow 3 exponent) * corner.orientation);
      )
    ) 0 corners in

    (* 2187 = 3^7 (7 are the corners and 3 because they are indexed in base 3) *)
    let index = (rank * 2187) + orientation_corners_num in
    index;


  method get_index_group_3 edges corners =
    let pairs = [| ULB , URF ; DLF , DRB ; URB , ULF ; DLB , DRF |] in
    let corners_pairs = get_tetrad_pairs corners pairs in 
    let corners_rank = pair_indexer corners_pairs in 
    let edges_map = [| UB; UR; UF; UL; DF; DL; DB; DR |] in 
    let edges_rank = combinaison_indexer (compute_edge_combo edges [| UB; UF; DF; DB |]) 12 4 in
    let parity = check_corners_parity corners in

    ((edges_rank * 2520 + corners_rank) * 2 + !parity);

end;;
