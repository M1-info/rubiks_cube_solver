open Utils_module.Types
open Utils_module.Utils
open Utils_module.Moves_store

(* 
  The cube is represented by 12 edges, 8 corners and 6 centers
  Each edge is represented by an edge enum and an orientation
  Each corner is represented by a corner enum and an orientation
  Each center is represented by a color enum

  Orientation are based from the solved state
  The orientation of an edge is 0 (not flipped) or 1 (flipped)
  The orientation of a corner is 0, 1 or 2

  We prefer to store the edge and corner enums instead of the index
  because it's easier to understand and to manipulate
  but we need to convert them to index to access the array
  so we have a function some functions to do that (see utils/utils.ml)
*)
class rubiks_cube = 
  object (self)

    val mutable edges   = Array.make 12 {e_enum = UB; orientation = 0}
    val mutable corners = Array.make 8  {c_enum = ULF; orientation = 0}
    val mutable centers = Array.make 6  {color = RED}

    (* Getters *)
    method get_edges () = edges
    method get_corners () = corners
    
    (* Initialize the cube at the solved state *)
    method init () = 
      for i = 0 to 11 do
        edges.(i) <- {e_enum = edge_enum_of_int i; orientation = 0}
      done;
      for i = 0 to 7 do
        corners.(i) <- {c_enum = corner_enum_of_int i; orientation = 0}
      done;
      for i = 0 to 5 do
        centers.(i) <- {color = color_of_int i}
      done;
      

    (* 
    Return array with the two edge colors
    If the orientation of the edge is flipped, the colors are flipped   
    *)
    method get_edge_colors index =
      let colors = Array.make 2 WHITE in
      let edge = edges.(index) in
      let set_colors first second =
        if edge.orientation = 0 then (
          colors.(0) <- first;
          colors.(1) <- second;
        ) else (
          colors.(0) <- second;
          colors.(1) <- first;
        )
      in 
      match edge.e_enum with
      | UB -> set_colors RED      YELLOW;   colors;
      | UR -> set_colors RED      GREEN;    colors;
      | UF -> set_colors RED      WHITE;    colors;
      | UL -> set_colors RED      BLUE;     colors;
      | FR -> set_colors WHITE    GREEN;    colors;
      | FL -> set_colors WHITE    BLUE;     colors;
      | BL -> set_colors YELLOW   BLUE;     colors;
      | BR -> set_colors YELLOW   GREEN;    colors;
      | DF -> set_colors ORANGE   WHITE;    colors;
      | DL -> set_colors ORANGE   BLUE;     colors;
      | DB -> set_colors ORANGE   YELLOW;   colors;
      | DR -> set_colors ORANGE   GREEN;    colors;
       
      
    (* 
      Return array with the three corner colors
      If the orientation of the corner is flipped, the colors are flipped too
    *)
    method get_corner_colors index =
      let colors = Array.make 3 WHITE in
      let corner = corners.(index) in
      let set_color first second third = 
        if corner.orientation = 0 then (
          colors.(0) <- first;
          colors.(1) <- second;
          colors.(2) <- third;

          if (int_of_corner_enum corner.c_enum + index) mod 2 == 1 then (
            let hold_color = colors.(1) in
            colors.(1) <- colors.(2);
            colors.(2) <- hold_color;
          )

        ) else if corner.orientation = 1 then (
          colors.(0) <- second;
          colors.(1) <- third;
          colors.(2) <- first;

          if (int_of_corner_enum corner.c_enum + index) mod 2 == 1 then (
            let hold_color = colors.(0) in
            colors.(0) <- colors.(1);
            colors.(1) <- hold_color;
          )

        ) else (
          colors.(0) <- third;
          colors.(1) <- first;
          colors.(2) <- second;

          if (int_of_corner_enum corner.c_enum + index) mod 2 == 1 then (
            let hold_color = colors.(0) in
            colors.(0) <- colors.(2);
            colors.(2) <- hold_color;
          )

        )
      in
      match corner.c_enum with
      | URF -> set_color RED      GREEN    WHITE;   colors;
      | ULF -> set_color RED      BLUE     WHITE;   colors;
      | ULB -> set_color RED      BLUE     YELLOW;  colors;
      | URB -> set_color RED      GREEN    YELLOW;  colors;
      | DRF -> set_color ORANGE   GREEN    WHITE;   colors;
      | DLF -> set_color ORANGE   BLUE     WHITE;   colors;
      | DLB -> set_color ORANGE   BLUE     YELLOW;  colors;
      | DRB -> set_color ORANGE   GREEN    YELLOW;  colors;


    method get_facette_color face row col = 
      let is_center = row = 1 && col = 1 in
      let is_out_bound = row < 0 || row > 2 || col < 0 || col > 2 in

      if is_center then centers.(int_of_face face).color
      else if is_out_bound then failwith "Invalid row or col"
      else (
        (* 
          Get corner colors return an array, 
          so we need to get the right index based on which face we are looking at
          Ex : UP, 0, 0 -> ULB -> 0 (U is for up)
        *)
        match face, row, col with 
        | UP, 0, 0 -> (self#get_corner_colors (int_of_corner_enum ULB)).(0)
        | UP, 0, 1 -> (self#get_edge_colors (int_of_edge_enum UB)).(0)
        | UP, 0, 2 -> (self#get_corner_colors (int_of_corner_enum URB)).(0)
        | UP, 1, 0 -> (self#get_edge_colors (int_of_edge_enum UL)).(0)
        | UP, 1, 2 -> (self#get_edge_colors (int_of_edge_enum UR)).(0)
        | UP, 2, 0 -> (self#get_corner_colors (int_of_corner_enum ULF)).(0)
        | UP, 2, 1 -> (self#get_edge_colors (int_of_edge_enum UF)).(0)
        | UP, 2, 2 -> (self#get_corner_colors (int_of_corner_enum URF)).(0)

        | RIGHT, 0, 0 -> (self#get_corner_colors (int_of_corner_enum URF)).(1)
        | RIGHT, 0, 1 -> (self#get_edge_colors (int_of_edge_enum UR)).(1)
        | RIGHT, 0, 2 -> (self#get_corner_colors (int_of_corner_enum URB)).(1)
        | RIGHT, 1, 0 -> (self#get_edge_colors (int_of_edge_enum FR)).(1)
        | RIGHT, 1, 2 -> (self#get_edge_colors (int_of_edge_enum BR)).(1)
        | RIGHT, 2, 0 -> (self#get_corner_colors (int_of_corner_enum DRF)).(1)
        | RIGHT, 2, 1 -> (self#get_edge_colors (int_of_edge_enum DR)).(1)
        | RIGHT, 2, 2 -> (self#get_corner_colors (int_of_corner_enum DRB)).(1)

        | FRONT, 0, 0 -> (self#get_corner_colors (int_of_corner_enum ULF)).(2)
        | FRONT, 0, 1 -> (self#get_edge_colors (int_of_edge_enum UF)).(1)
        | FRONT, 0, 2 -> (self#get_corner_colors (int_of_corner_enum URF)).(2)
        | FRONT, 1, 0 -> (self#get_edge_colors (int_of_edge_enum FL)).(0)
        | FRONT, 1, 2 -> (self#get_edge_colors (int_of_edge_enum FR)).(0)
        | FRONT, 2, 0 -> (self#get_corner_colors (int_of_corner_enum DLF)).(2)
        | FRONT, 2, 1 -> (self#get_edge_colors (int_of_edge_enum DF)).(1)
        | FRONT, 2, 2 -> (self#get_corner_colors (int_of_corner_enum DRF)).(2)

        | LEFT, 0, 0 -> (self#get_corner_colors (int_of_corner_enum ULB)).(1)
        | LEFT, 0, 1 -> (self#get_edge_colors (int_of_edge_enum UL)).(1)
        | LEFT, 0, 2 -> (self#get_corner_colors (int_of_corner_enum ULF)).(1)
        | LEFT, 1, 0 -> (self#get_edge_colors (int_of_edge_enum BL)).(1)
        | LEFT, 1, 2 -> (self#get_edge_colors (int_of_edge_enum FL)).(1)
        | LEFT, 2, 0 -> (self#get_corner_colors (int_of_corner_enum DLB)).(1)
        | LEFT, 2, 1 -> (self#get_edge_colors (int_of_edge_enum DL)).(1)
        | LEFT, 2, 2 -> (self#get_corner_colors (int_of_corner_enum DLF)).(1)

        | BACK, 0, 0 -> (self#get_corner_colors (int_of_corner_enum URB)).(2)
        | BACK, 0, 1 -> (self#get_edge_colors (int_of_edge_enum UB)).(1)
        | BACK, 0, 2 -> (self#get_corner_colors (int_of_corner_enum ULB)).(2)
        | BACK, 1, 0 -> (self#get_edge_colors (int_of_edge_enum BR)).(0)
        | BACK, 1, 2 -> (self#get_edge_colors (int_of_edge_enum BL)).(0)
        | BACK, 2, 0 -> (self#get_corner_colors (int_of_corner_enum DRB)).(2)
        | BACK, 2, 1 -> (self#get_edge_colors (int_of_edge_enum DB)).(1)
        | BACK, 2, 2 -> (self#get_corner_colors (int_of_corner_enum DLB)).(2)

        | DOWN, 0, 0 -> (self#get_corner_colors (int_of_corner_enum DLF)).(0)
        | DOWN, 0, 1 -> (self#get_edge_colors (int_of_edge_enum DF)).(0)
        | DOWN, 0, 2 -> (self#get_corner_colors (int_of_corner_enum DRF)).(0)
        | DOWN, 1, 0 -> (self#get_edge_colors (int_of_edge_enum DL)).(0)
        | DOWN, 1, 2 -> (self#get_edge_colors (int_of_edge_enum DR)).(0)
        | DOWN, 2, 0 -> (self#get_corner_colors (int_of_corner_enum DLB)).(0)
        | DOWN, 2, 1 -> (self#get_edge_colors (int_of_edge_enum DB)).(0)
        | DOWN, 2, 2 -> (self#get_corner_colors (int_of_corner_enum DRB)).(0)

        | _, _, _ -> failwith "Invalid face or position"
      )

    method get_edge_index edge = 
      let nb_edges = Array.length edges in
      let rec get_index index = 
        if index = nb_edges then failwith "Invalid edge"
        else if edges.(index).e_enum = edge then index
        else get_index (index + 1)
      in get_index 0

    
    method get_corner_index corner =
      let nb_corners = Array.length corners in
      let rec get_index index = 
        if index = nb_corners then failwith "Invalid corner"
        else if corners.(index).c_enum = corner then index
        else get_index (index + 1)
      in get_index 0

    (*
      Return a boolean indicating if the cube is solved or not.
      To do this, we check if the edges and corners are in the right position and if the orientation is correct.
      Position are based from the order in the enum (see utils/types.ml)
      Orientation is good if it is equal to 0.
    *)
    method is_solved () =
      let rec check_edge index = 
        if index = Array.length edges then true
        else if edges.(index).e_enum <> (edge_enum_of_int index) || edges.(index).orientation <> 0 then false
        else check_edge (index + 1)
      and check_corners index = 
        if index = Array.length corners then true
        else if corners.(index).c_enum <> (corner_enum_of_int index) || corners.(index).orientation <> 0 then false
        else check_corners (index + 1)
      in check_edge 0 && check_corners 0


    (*
      In some moves, the orientation of the corners. 
      This method updates the orientation of the corners.   
    *)
    method update_corner_orientation corner_enum delta = 
      let corner = corners.(int_of_corner_enum corner_enum) in
      corner.orientation <- (corner.orientation + delta) mod 3;


    (*
      In some moves, the orientation of the edges. 
      This method updates the orientation of the edges.
      We use a bit to represent the orientation. 
      So we use xor to update the orientation to make it faster.
    *)
    method update_edge_orientation edge_enum = 
      let edge = edges.(int_of_edge_enum edge_enum) in
      let orientation = edge.orientation in
      edge.orientation <- orientation lxor 1;


    (* Cube moves *)
    
    method u () = 
      let hold_corner = corners.(int_of_corner_enum ULF) in
      corners.(int_of_corner_enum ULF) <- corners.(int_of_corner_enum URF);
      corners.(int_of_corner_enum URF) <- corners.(int_of_corner_enum URB);
      corners.(int_of_corner_enum URB) <- corners.(int_of_corner_enum ULB);
      corners.(int_of_corner_enum ULB) <- hold_corner;

      let hold_edge = edges.(int_of_edge_enum UL) in
      edges.(int_of_edge_enum UL) <- edges.(int_of_edge_enum UF);
      edges.(int_of_edge_enum UF) <- edges.(int_of_edge_enum UR);
      edges.(int_of_edge_enum UR) <- edges.(int_of_edge_enum UB);
      edges.(int_of_edge_enum UB) <- hold_edge; 


    method u_prime () =
      let hold_corner = corners.(int_of_corner_enum ULB) in
      corners.(int_of_corner_enum ULB) <- corners.(int_of_corner_enum URB);
      corners.(int_of_corner_enum URB) <- corners.(int_of_corner_enum URF);
      corners.(int_of_corner_enum URF) <- corners.(int_of_corner_enum ULF);
      corners.(int_of_corner_enum ULF) <- hold_corner;

      let hold_edge = edges.(int_of_edge_enum UB) in
      edges.(int_of_edge_enum UB) <- edges.(int_of_edge_enum UR);
      edges.(int_of_edge_enum UR) <- edges.(int_of_edge_enum UF);
      edges.(int_of_edge_enum UF) <- edges.(int_of_edge_enum UL);
      edges.(int_of_edge_enum UL) <- hold_edge;

    
    method u_2 () = 
      let hold_corner = corners.(int_of_corner_enum ULB) in
      corners.(int_of_corner_enum ULB) <- corners.(int_of_corner_enum URF);
      corners.(int_of_corner_enum URF) <- hold_corner;

      let hold_corner = corners.(int_of_corner_enum URB) in
      corners.(int_of_corner_enum URB) <- corners.(int_of_corner_enum ULF);
      corners.(int_of_corner_enum ULF) <- hold_corner;

      let hold_edge = edges.(int_of_edge_enum UB) in
      edges.(int_of_edge_enum UB) <- edges.(int_of_edge_enum UF);
      edges.(int_of_edge_enum UF) <- hold_edge;

      let hold_edge = edges.(int_of_edge_enum UR) in
      edges.(int_of_edge_enum UR) <- edges.(int_of_edge_enum UL);
      edges.(int_of_edge_enum UL) <- hold_edge;

    
    method l () = 
      let hold_corner = corners.(int_of_corner_enum DLB) in
      corners.(int_of_corner_enum DLB) <- corners.(int_of_corner_enum DLF);
      corners.(int_of_corner_enum DLF) <- corners.(int_of_corner_enum ULF);
      corners.(int_of_corner_enum ULF) <- corners.(int_of_corner_enum ULB);
      corners.(int_of_corner_enum ULB) <- hold_corner;

      let hold_edge = edges.(int_of_edge_enum BL) in
      edges.(int_of_edge_enum BL) <- edges.(int_of_edge_enum DL);
      edges.(int_of_edge_enum DL) <- edges.(int_of_edge_enum FL);
      edges.(int_of_edge_enum FL) <- edges.(int_of_edge_enum UL);
      edges.(int_of_edge_enum UL) <- hold_edge;

      self#update_corner_orientation DLB 1;
      self#update_corner_orientation DLF 1;
      self#update_corner_orientation ULF 1;
      self#update_corner_orientation ULB 1;


    method l_prime () =
      let hold_corner = corners.(int_of_corner_enum DLB) in
      corners.(int_of_corner_enum DLB) <- corners.(int_of_corner_enum ULB);
      corners.(int_of_corner_enum ULB) <- corners.(int_of_corner_enum ULF);
      corners.(int_of_corner_enum ULF) <- corners.(int_of_corner_enum DLF);
      corners.(int_of_corner_enum DLF) <- hold_corner;

      let hold_edge = edges.(int_of_edge_enum BL) in
      edges.(int_of_edge_enum BL) <- edges.(int_of_edge_enum UL);
      edges.(int_of_edge_enum UL) <- edges.(int_of_edge_enum FL);
      edges.(int_of_edge_enum FL) <- edges.(int_of_edge_enum DL);
      edges.(int_of_edge_enum DL) <- hold_edge;

      self#update_corner_orientation DLB 1;
      self#update_corner_orientation DLF 1;
      self#update_corner_orientation ULF 1;
      self#update_corner_orientation ULB 1;


    method l_2 () = 
      let hold_corner = corners.(int_of_corner_enum DLB) in
      corners.(int_of_corner_enum DLB) <- corners.(int_of_corner_enum ULF);
      corners.(int_of_corner_enum ULF) <- hold_corner;

      let hold_corner = corners.(int_of_corner_enum DLF) in
      corners.(int_of_corner_enum DLF) <- corners.(int_of_corner_enum ULB);
      corners.(int_of_corner_enum ULB) <- hold_corner;

      let hold_edge = edges.(int_of_edge_enum BL) in
      edges.(int_of_edge_enum BL) <- edges.(int_of_edge_enum FL);
      edges.(int_of_edge_enum FL) <- hold_edge;

      let hold_edge = edges.(int_of_edge_enum DL) in
      edges.(int_of_edge_enum DL) <- edges.(int_of_edge_enum UL);
      edges.(int_of_edge_enum UL) <- hold_edge;


    method f () = 
      let hold_corner = corners.(int_of_corner_enum ULF) in
      corners.(int_of_corner_enum ULF) <- corners.(int_of_corner_enum DLF);
      corners.(int_of_corner_enum DLF) <- corners.(int_of_corner_enum DRF);
      corners.(int_of_corner_enum DRF) <- corners.(int_of_corner_enum URF);
      corners.(int_of_corner_enum URF) <- hold_corner;

      let hold_edge = edges.(int_of_edge_enum UF) in
      edges.(int_of_edge_enum UF) <- edges.(int_of_edge_enum FL);
      edges.(int_of_edge_enum FL) <- edges.(int_of_edge_enum DF);
      edges.(int_of_edge_enum DF) <- edges.(int_of_edge_enum FR);
      edges.(int_of_edge_enum FR) <- hold_edge;

      self#update_corner_orientation ULF 2;
      self#update_corner_orientation URF 2;
      self#update_corner_orientation DRF 2;
      self#update_corner_orientation DLF 2;

      self#update_edge_orientation UF;
      self#update_edge_orientation FL;
      self#update_edge_orientation DF;
      self#update_edge_orientation FR;


  method f_prime () = 
    let hold_corner = corners.(int_of_corner_enum ULF) in
      corners.(int_of_corner_enum ULF) <- corners.(int_of_corner_enum URF);
      corners.(int_of_corner_enum URF) <- corners.(int_of_corner_enum DRF);
      corners.(int_of_corner_enum DRF) <- corners.(int_of_corner_enum DLF);
      corners.(int_of_corner_enum DLF) <- hold_corner;

      let hold_edge = edges.(int_of_edge_enum UF) in
      edges.(int_of_edge_enum UF) <- edges.(int_of_edge_enum FR);
      edges.(int_of_edge_enum FR) <- edges.(int_of_edge_enum DF);
      edges.(int_of_edge_enum DF) <- edges.(int_of_edge_enum FL);
      edges.(int_of_edge_enum FL) <- hold_edge;

      self#update_corner_orientation ULF 2;
      self#update_corner_orientation URF 2;
      self#update_corner_orientation DRF 2;
      self#update_corner_orientation DLF 2;

      self#update_edge_orientation UF;
      self#update_edge_orientation DF;
      self#update_edge_orientation FR;
      self#update_edge_orientation FL;


    method f_2 () = 
      let hold_corner = corners.(int_of_corner_enum ULF) in
      corners.(int_of_corner_enum ULF) <- corners.(int_of_corner_enum DRF);
      corners.(int_of_corner_enum DRF) <- hold_corner;

      let hold_corner = corners.(int_of_corner_enum URF) in
      corners.(int_of_corner_enum URF) <- corners.(int_of_corner_enum DLF);
      corners.(int_of_corner_enum DLF) <- hold_corner;

      let hold_edge = edges.(int_of_edge_enum UF) in
      edges.(int_of_edge_enum UF) <- edges.(int_of_edge_enum DF);
      edges.(int_of_edge_enum DF) <- hold_edge;

      let hold_edge = edges.(int_of_edge_enum FL) in
      edges.(int_of_edge_enum FL) <- edges.(int_of_edge_enum FR);
      edges.(int_of_edge_enum FR) <- hold_edge;


    method r () = 
      let hold_corner = corners.(int_of_corner_enum DRB) in
      corners.(int_of_corner_enum DRB) <- corners.(int_of_corner_enum URB);
      corners.(int_of_corner_enum URB) <- corners.(int_of_corner_enum URF);
      corners.(int_of_corner_enum URF) <- corners.(int_of_corner_enum DRF);
      corners.(int_of_corner_enum DRF) <- hold_corner;

      let hold_edge = edges.(int_of_edge_enum BR) in
      edges.(int_of_edge_enum BR) <- edges.(int_of_edge_enum UR);
      edges.(int_of_edge_enum UR) <- edges.(int_of_edge_enum FR);
      edges.(int_of_edge_enum FR) <- edges.(int_of_edge_enum DR);
      edges.(int_of_edge_enum DR) <- hold_edge;

      self#update_corner_orientation DRB 1;
      self#update_corner_orientation DRF 1;
      self#update_corner_orientation URF 1;
      self#update_corner_orientation URB 1;

    
    method r_prime () = 
      let hold_corner = corners.(int_of_corner_enum DRB) in
      corners.(int_of_corner_enum DRB) <- corners.(int_of_corner_enum DRF);
      corners.(int_of_corner_enum DRF) <- corners.(int_of_corner_enum URF);
      corners.(int_of_corner_enum URF) <- corners.(int_of_corner_enum URB);
      corners.(int_of_corner_enum URB) <- hold_corner;

      let hold_edge = edges.(int_of_edge_enum BR) in
      edges.(int_of_edge_enum BR) <- edges.(int_of_edge_enum DR);
      edges.(int_of_edge_enum DR) <- edges.(int_of_edge_enum FR);
      edges.(int_of_edge_enum FR) <- edges.(int_of_edge_enum UR);
      edges.(int_of_edge_enum UR) <- hold_edge;

      self#update_corner_orientation DRB 1;
      self#update_corner_orientation DRF 1;
      self#update_corner_orientation URF 1;
      self#update_corner_orientation URB 1;


    method r_2 () =

      let hold_corner = corners.(int_of_corner_enum DRB) in
      corners.(int_of_corner_enum DRB) <- corners.(int_of_corner_enum URF);
      corners.(int_of_corner_enum URF) <- hold_corner;

      let hold_corner = corners.(int_of_corner_enum URB) in
      corners.(int_of_corner_enum URB) <- corners.(int_of_corner_enum DRF);
      corners.(int_of_corner_enum DRF) <- hold_corner;

      let hold_edge = edges.(int_of_edge_enum BR) in
      edges.(int_of_edge_enum BR) <- edges.(int_of_edge_enum FR);
      edges.(int_of_edge_enum FR) <- hold_edge;

      let hold_edge = edges.(int_of_edge_enum UR) in
      edges.(int_of_edge_enum UR) <- edges.(int_of_edge_enum DR);
      edges.(int_of_edge_enum DR) <- hold_edge;

    
    method b () = 
      let hold_corner = corners.(int_of_corner_enum ULB) in
      corners.(int_of_corner_enum ULB) <- corners.(int_of_corner_enum URB);
      corners.(int_of_corner_enum URB) <- corners.(int_of_corner_enum DRB);
      corners.(int_of_corner_enum DRB) <- corners.(int_of_corner_enum DLB);
      corners.(int_of_corner_enum DLB) <- hold_corner;

      let hold_edge = edges.(int_of_edge_enum UB) in
      edges.(int_of_edge_enum UB) <- edges.(int_of_edge_enum BR);
      edges.(int_of_edge_enum BR) <- edges.(int_of_edge_enum DB);
      edges.(int_of_edge_enum DB) <- edges.(int_of_edge_enum BL);
      edges.(int_of_edge_enum BL) <- hold_edge;

      self#update_corner_orientation ULB 2;
      self#update_corner_orientation URB 2;
      self#update_corner_orientation DRB 2;
      self#update_corner_orientation DLB 2;

      self#update_edge_orientation UB;
      self#update_edge_orientation BR;
      self#update_edge_orientation DB;
      self#update_edge_orientation BL;


    method b_prime () =
      let hold_corner = corners.(int_of_corner_enum ULB) in
      corners.(int_of_corner_enum ULB) <- corners.(int_of_corner_enum DLB);
      corners.(int_of_corner_enum DLB) <- corners.(int_of_corner_enum DRB);
      corners.(int_of_corner_enum DRB) <- corners.(int_of_corner_enum URB);
      corners.(int_of_corner_enum URB) <- hold_corner;

      let hold_edge = edges.(int_of_edge_enum UB) in
      edges.(int_of_edge_enum UB) <- edges.(int_of_edge_enum BL);
      edges.(int_of_edge_enum BL) <- edges.(int_of_edge_enum DB);
      edges.(int_of_edge_enum DB) <- edges.(int_of_edge_enum BR);
      edges.(int_of_edge_enum BR) <- hold_edge;

      self#update_corner_orientation ULB 2;
      self#update_corner_orientation URB 2;
      self#update_corner_orientation DRB 2;
      self#update_corner_orientation DLB 2;

      self#update_edge_orientation UB;
      self#update_edge_orientation BR;
      self#update_edge_orientation DB;
      self#update_edge_orientation BL;


    method b_2 () =
      let hold_corner = corners.(int_of_corner_enum ULB) in
      corners.(int_of_corner_enum ULB) <- corners.(int_of_corner_enum DRB);
      corners.(int_of_corner_enum DRB) <- hold_corner;

      let hold_corner = corners.(int_of_corner_enum URB) in
      corners.(int_of_corner_enum URB) <- corners.(int_of_corner_enum DLB);
      corners.(int_of_corner_enum DLB) <- hold_corner;

      let hold_edge = edges.(int_of_edge_enum UB) in
      edges.(int_of_edge_enum UB) <- edges.(int_of_edge_enum DB);
      edges.(int_of_edge_enum DB) <- hold_edge;

      let hold_edge = edges.(int_of_edge_enum BL) in
      edges.(int_of_edge_enum BL) <- edges.(int_of_edge_enum BR);
      edges.(int_of_edge_enum BR) <- hold_edge;


    method d () = 
      let hold_corner = corners.(int_of_corner_enum DLB) in
      corners.(int_of_corner_enum DLB) <- corners.(int_of_corner_enum DRB);
      corners.(int_of_corner_enum DRB) <- corners.(int_of_corner_enum DRF);
      corners.(int_of_corner_enum DRF) <- corners.(int_of_corner_enum DLF);
      corners.(int_of_corner_enum DLF) <- hold_corner;

      let hold_edge = edges.(int_of_edge_enum DB) in
      edges.(int_of_edge_enum DB) <- edges.(int_of_edge_enum DR);
      edges.(int_of_edge_enum DR) <- edges.(int_of_edge_enum DF);
      edges.(int_of_edge_enum DF) <- edges.(int_of_edge_enum DL);
      edges.(int_of_edge_enum DL) <- hold_edge;

    
    method d_prime () = 
      let hold_corner = corners.(int_of_corner_enum DLF) in
      corners.(int_of_corner_enum DLF) <- corners.(int_of_corner_enum DRF);
      corners.(int_of_corner_enum DRF) <- corners.(int_of_corner_enum DRB);
      corners.(int_of_corner_enum DRB) <- corners.(int_of_corner_enum DLB);
      corners.(int_of_corner_enum DLB) <- hold_corner;

      let hold_edge = edges.(int_of_edge_enum DL) in
      edges.(int_of_edge_enum DL) <- edges.(int_of_edge_enum DF);
      edges.(int_of_edge_enum DF) <- edges.(int_of_edge_enum DR);
      edges.(int_of_edge_enum DR) <- edges.(int_of_edge_enum DB);
      edges.(int_of_edge_enum DB) <- hold_edge;


    method d_2 () = 
      let hold_corner = corners.(int_of_corner_enum DLB) in
      corners.(int_of_corner_enum DLB) <- corners.(int_of_corner_enum DRF);
      corners.(int_of_corner_enum DRF) <- hold_corner;

      let hold_corner = corners.(int_of_corner_enum DRB) in
      corners.(int_of_corner_enum DRB) <- corners.(int_of_corner_enum DLF);
      corners.(int_of_corner_enum DLF) <- hold_corner;

      let hold_edge = edges.(int_of_edge_enum DB) in
      edges.(int_of_edge_enum DB) <- edges.(int_of_edge_enum DF);
      edges.(int_of_edge_enum DF) <- hold_edge;

      let hold_edge = edges.(int_of_edge_enum DR) in
      edges.(int_of_edge_enum DR) <- edges.(int_of_edge_enum DL);
      edges.(int_of_edge_enum DL) <- hold_edge;

    
    method apply_move move_string = 
      match move_string with 
      | "u"       -> self#u ()
      | "u_prime" -> self#u_prime ()
      | "u_2"     -> self#u_2 ()
      | "r"       -> self#r ()
      | "r_prime" -> self#r_prime ()
      | "r_2"     -> self#r_2 ()
      | "f"       -> self#f ()
      | "f_prime" -> self#f_prime ()
      | "f_2"     -> self#f_2 ()
      | "l"       -> self#l ()
      | "l_prime" -> self#l_prime ()
      | "l_2"     -> self#l_2 ()
      | "b"       -> self#b ()
      | "b_prime" -> self#b_prime ()
      | "b_2"     -> self#b_2 ()
      | "d"       -> self#d ()
      | "d_prime" -> self#d_prime ()
      | "d_2"     -> self#d_2 ()
      | _ -> ()


    method scramble num_moves =
      Random.self_init ();
      let nb_moves = Array.length all_moves in 
      let rec scramble_aux index =
        if index = num_moves then ()
        else (
          let random = Random.int nb_moves in
          let move = all_moves.(random) in
          let move_string = string_of_move move in
          print_string move_string;
          print_string " ";
          self#apply_move move_string;
          scramble_aux (index + 1)
        )
      in scramble_aux 0; 
          
  end;;

