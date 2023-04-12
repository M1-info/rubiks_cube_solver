open Utils_module.Utils
open Utils_module.Functions
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

  method get_index_group_1 edges = 
    let nb_edges = Array.length edges in
    let index = Array.fold_left (fun acc edge -> 
        let index = get_edge_index edge in 
        let exponent = Int.abs (nb_edges - index) in
        acc + ((pow 2 exponent) * edge.orientation);
    ) 0 edges in 
    index;
    
  end;;
