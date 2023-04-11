open Utils_module.Types;;
open Utils_module.Utils;;

class rubiks_cube = 
  object (self)
    val mutable edges = Array.make 12 {edge = UB; orientation = 0}
    val mutable corners = Array.make 8 {corner = ULF; orientation = 0}
    val mutable centers = Array.make 6 {color = RED}
    
    method init () = 
      for i = 0 to 11 do
        edges.(i) <- {edge = get_edge_from_index(i); orientation = 0}
      done;
      for i = 0 to 7 do
        corners.(i) <- {corner = get_corner_from_index(i); orientation = 0}
      done;
      for i = 0 to 5 do
        centers.(i) <- {color = color_from_int(i)}
      done;

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
      match edge.edge with
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


    method get_corner_colors index =
      let colors = Array.make 3 WHITE in
      let corner = corners.(index) in
      let set_color first second third = 
        if corner.orientation = 0 then (
          colors.(0) <- first;
          colors.(1) <- second;
          colors.(2) <- third;
        ) else if corner.orientation = 1 then (
          colors.(0) <- second;
          colors.(1) <- third;
          colors.(2) <- first;
        ) else (
          colors.(0) <- third;
          colors.(1) <- first;
          colors.(2) <- second;
        )
      in
      match corner.corner with
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
        match face, row, col with 
        | UP, 0, 0 -> (self#get_corner_colors (get_corner_from_enum ULB)).(0)
        | UP, 0, 1 -> (self#get_edge_colors (get_edge_from_enum UB)).(0)
        | UP, 0, 2 -> (self#get_corner_colors (get_corner_from_enum URB)).(0)
        | UP, 1, 0 -> (self#get_edge_colors (get_edge_from_enum UL)).(0)
        | UP, 1, 2 -> (self#get_edge_colors (get_edge_from_enum UR)).(0)
        | UP, 2, 0 -> (self#get_corner_colors (get_corner_from_enum ULF)).(0)
        | UP, 2, 1 -> (self#get_edge_colors (get_edge_from_enum UF)).(0)
        | UP, 2, 2 -> (self#get_corner_colors (get_corner_from_enum URF)).(0)

        | RIGHT, 0, 0 -> (self#get_corner_colors (get_corner_from_enum URF)).(1)
        | RIGHT, 0, 1 -> (self#get_edge_colors (get_edge_from_enum UR)).(1)
        | RIGHT, 0, 2 -> (self#get_corner_colors (get_corner_from_enum URB)).(1)
        | RIGHT, 1, 0 -> (self#get_edge_colors (get_edge_from_enum FR)).(1)
        | RIGHT, 1, 2 -> (self#get_edge_colors (get_edge_from_enum BR)).(1)
        | RIGHT, 2, 0 -> (self#get_corner_colors (get_corner_from_enum DRF)).(1)
        | RIGHT, 2, 1 -> (self#get_edge_colors (get_edge_from_enum DR)).(1)
        | RIGHT, 2, 2 -> (self#get_corner_colors (get_corner_from_enum DRB)).(1)

        | FRONT, 0, 0 -> (self#get_corner_colors (get_corner_from_enum ULF)).(2)
        | FRONT, 0, 1 -> (self#get_edge_colors (get_edge_from_enum UF)).(1)
        | FRONT, 0, 2 -> (self#get_corner_colors (get_corner_from_enum URF)).(2)
        | FRONT, 1, 0 -> (self#get_edge_colors (get_edge_from_enum FL)).(0)
        | FRONT, 1, 2 -> (self#get_edge_colors (get_edge_from_enum FR)).(0)
        | FRONT, 2, 0 -> (self#get_corner_colors (get_corner_from_enum DLF)).(2)
        | FRONT, 2, 1 -> (self#get_edge_colors (get_edge_from_enum DF)).(1)
        | FRONT, 2, 2 -> (self#get_corner_colors (get_corner_from_enum DRF)).(2)

        | LEFT, 0, 0 -> (self#get_corner_colors (get_corner_from_enum ULB)).(1)
        | LEFT, 0, 1 -> (self#get_edge_colors (get_edge_from_enum UL)).(1)
        | LEFT, 0, 2 -> (self#get_corner_colors (get_corner_from_enum ULF)).(1)
        | LEFT, 1, 0 -> (self#get_edge_colors (get_edge_from_enum BL)).(1)
        | LEFT, 1, 2 -> (self#get_edge_colors (get_edge_from_enum FL)).(1)
        | LEFT, 2, 0 -> (self#get_corner_colors (get_corner_from_enum DLB)).(1)
        | LEFT, 2, 1 -> (self#get_edge_colors (get_edge_from_enum DL)).(1)
        | LEFT, 2, 2 -> (self#get_corner_colors (get_corner_from_enum DLF)).(1)

        | BACK, 0, 0 -> (self#get_corner_colors (get_corner_from_enum URB)).(2)
        | BACK, 0, 1 -> (self#get_edge_colors (get_edge_from_enum UB)).(1)
        | BACK, 0, 2 -> (self#get_corner_colors (get_corner_from_enum ULB)).(2)
        | BACK, 1, 0 -> (self#get_edge_colors (get_edge_from_enum BR)).(0)
        | BACK, 1, 2 -> (self#get_edge_colors (get_edge_from_enum BL)).(0)
        | BACK, 2, 0 -> (self#get_corner_colors (get_corner_from_enum DRB)).(2)
        | BACK, 2, 1 -> (self#get_edge_colors (get_edge_from_enum DB)).(1)
        | BACK, 2, 2 -> (self#get_corner_colors (get_corner_from_enum DLB)).(2)

        | DOWN, 0, 0 -> (self#get_corner_colors (get_corner_from_enum DLF)).(0)
        | DOWN, 0, 1 -> (self#get_edge_colors (get_edge_from_enum DF)).(0)
        | DOWN, 0, 2 -> (self#get_corner_colors (get_corner_from_enum DRF)).(0)
        | DOWN, 1, 0 -> (self#get_edge_colors (get_edge_from_enum DL)).(0)
        | DOWN, 1, 2 -> (self#get_edge_colors (get_edge_from_enum DR)).(0)
        | DOWN, 2, 0 -> (self#get_corner_colors (get_corner_from_enum DLB)).(0)
        | DOWN, 2, 1 -> (self#get_edge_colors (get_edge_from_enum DB)).(0)
        | DOWN, 2, 2 -> (self#get_corner_colors (get_corner_from_enum DRB)).(0)

        | _, _, _ -> failwith "Invalid face or position"
      )


    method is_solved () =
      let rec check_edge index = 
        if index = Array.length edges then true
        else if edges.(index).edge <> (get_edge_from_index index) || edges.(index).orientation <> 0 then false
        else check_edge (index + 1)
      and check_corners index = 
        if index = Array.length corners then true
        else if corners.(index).corner <> (get_corner_from_index index) || corners.(index).orientation <> 0 then false
        else check_corners (index + 1)
      in check_edge 0 && check_corners 0

    
    method update_corner_orientation corner_enum delta = 
      let corner = corners.(get_corner_from_enum corner_enum) in
      corner.orientation <- corner.orientation + delta;
      if corner.orientation = 3 then corner.orientation <- 0;
      if corner.orientation = 4 then corner.orientation <- 1;

    
    method u () = 
      let hold_corner = corners.(get_corner_from_enum ULF) in
      corners.(get_corner_from_enum ULF) <- corners.(get_corner_from_enum URF);
      corners.(get_corner_from_enum URF) <- corners.(get_corner_from_enum URB);
      corners.(get_corner_from_enum URB) <- corners.(get_corner_from_enum ULB);
      corners.(get_corner_from_enum ULB) <- hold_corner;

      let hold_edge = edges.(get_edge_from_enum UL) in
      edges.(get_edge_from_enum UL) <- edges.(get_edge_from_enum UF);
      edges.(get_edge_from_enum UF) <- edges.(get_edge_from_enum UR);
      edges.(get_edge_from_enum UR) <- edges.(get_edge_from_enum UB);
      edges.(get_edge_from_enum UB) <- hold_edge;


    method u_prime () =
      let hold_corner = corners.(get_corner_from_enum ULB) in
      corners.(get_corner_from_enum ULB) <- corners.(get_corner_from_enum URB);
      corners.(get_corner_from_enum URB) <- corners.(get_corner_from_enum URF);
      corners.(get_corner_from_enum URF) <- corners.(get_corner_from_enum ULF);
      corners.(get_corner_from_enum ULF) <- hold_corner;

      let hold_edge = edges.(get_edge_from_enum UB) in
      edges.(get_edge_from_enum UB) <- edges.(get_edge_from_enum UR);
      edges.(get_edge_from_enum UR) <- edges.(get_edge_from_enum UF);
      edges.(get_edge_from_enum UF) <- edges.(get_edge_from_enum UL);
      edges.(get_edge_from_enum UL) <- hold_edge;

    
    method u_2 () = 
      let hold_corner = corners.(get_corner_from_enum ULB) in
      corners.(get_corner_from_enum ULB) <- corners.(get_corner_from_enum URF);
      corners.(get_corner_from_enum URF) <- hold_corner;

      let hold_corner = corners.(get_corner_from_enum URB) in
      corners.(get_corner_from_enum URB) <- corners.(get_corner_from_enum ULF);
      corners.(get_corner_from_enum ULF) <- hold_corner;

      let hold_edge = edges.(get_edge_from_enum UB) in
      edges.(get_edge_from_enum UB) <- edges.(get_edge_from_enum UF);
      edges.(get_edge_from_enum UF) <- hold_edge;

      let hold_edge = edges.(get_edge_from_enum UR) in
      edges.(get_edge_from_enum UR) <- edges.(get_edge_from_enum UL);
      edges.(get_edge_from_enum UL) <- hold_edge;

    
    method l () = 
      let hold_corner = corners.(get_corner_from_enum DLB) in
      corners.(get_corner_from_enum DLB) <- corners.(get_corner_from_enum DLF);
      corners.(get_corner_from_enum DLF) <- corners.(get_corner_from_enum ULF);
      corners.(get_corner_from_enum ULF) <- corners.(get_corner_from_enum ULB);
      corners.(get_corner_from_enum ULB) <- hold_corner;

      let hold_edge = edges.(get_edge_from_enum BL) in
      edges.(get_edge_from_enum BL) <- edges.(get_edge_from_enum DL);
      edges.(get_edge_from_enum DL) <- edges.(get_edge_from_enum FL);
      edges.(get_edge_from_enum FL) <- edges.(get_edge_from_enum UL);
      edges.(get_edge_from_enum UL) <- hold_edge;

      self#update_corner_orientation DLB 1;
      self#update_corner_orientation DLF 2;
      self#update_corner_orientation ULF 1;
      self#update_corner_orientation ULB 2;


    method l_prime () =
      let hold_corner = corners.(get_corner_from_enum DLB) in
      corners.(get_corner_from_enum DLB) <- corners.(get_corner_from_enum ULB);
      corners.(get_corner_from_enum ULB) <- corners.(get_corner_from_enum ULF);
      corners.(get_corner_from_enum ULF) <- corners.(get_corner_from_enum DLF);
      corners.(get_corner_from_enum DLF) <- hold_corner;

      let hold_edge = edges.(get_edge_from_enum BL) in
      edges.(get_edge_from_enum BL) <- edges.(get_edge_from_enum UL);
      edges.(get_edge_from_enum UL) <- edges.(get_edge_from_enum FL);
      edges.(get_edge_from_enum FL) <- edges.(get_edge_from_enum DL);
      edges.(get_edge_from_enum DL) <- hold_edge;

      self#update_corner_orientation DLB 1;
      self#update_corner_orientation DLF 2;
      self#update_corner_orientation ULF 1;
      self#update_corner_orientation ULB 2;


    method l_2 () = 
      let hold_corner = corners.(get_corner_from_enum DLB) in
      corners.(get_corner_from_enum DLB) <- corners.(get_corner_from_enum ULF);
      corners.(get_corner_from_enum ULF) <- hold_corner;

      let hold_corner = corners.(get_corner_from_enum DLF) in
      corners.(get_corner_from_enum DLF) <- corners.(get_corner_from_enum ULB);
      corners.(get_corner_from_enum ULB) <- hold_corner;

      let hold_edge = edges.(get_edge_from_enum BL) in
      edges.(get_edge_from_enum BL) <- edges.(get_edge_from_enum FL);
      edges.(get_edge_from_enum FL) <- hold_edge;

      let hold_edge = edges.(get_edge_from_enum DL) in
      edges.(get_edge_from_enum DL) <- edges.(get_edge_from_enum UL);
      edges.(get_edge_from_enum UL) <- hold_edge;


    method f () = 
      let hold_corner = corners.(get_corner_from_enum ULF) in
      corners.(get_corner_from_enum ULF) <- corners.(get_corner_from_enum DLF);
      corners.(get_corner_from_enum DLF) <- corners.(get_corner_from_enum DRF);
      corners.(get_corner_from_enum DRF) <- corners.(get_corner_from_enum URF);
      corners.(get_corner_from_enum URF) <- hold_corner;

      let hold_edge = edges.(get_edge_from_enum UF) in
      edges.(get_edge_from_enum UF) <- edges.(get_edge_from_enum FL);
      edges.(get_edge_from_enum FL) <- edges.(get_edge_from_enum DF);
      edges.(get_edge_from_enum DF) <- edges.(get_edge_from_enum FR);
      edges.(get_edge_from_enum FR) <- hold_edge;

      self#update_corner_orientation ULF 2;
      self#update_corner_orientation URF 1;
      self#update_corner_orientation DRF 2;
      self#update_corner_orientation DLF 1;

      let new_edge = get_edge_from_enum (edges.(get_edge_from_enum UF).edge) lxor 1 in
      edges.(get_edge_from_enum UF) <- {edge = get_edge_from_index new_edge; orientation = 0};

      let new_edge = get_edge_from_enum (edges.(get_edge_from_enum FL).edge) lxor 1 in
      edges.(get_edge_from_enum FL) <- {edge = get_edge_from_index new_edge; orientation = 0};

      let new_edge = get_edge_from_enum (edges.(get_edge_from_enum DF).edge) lxor 1 in
      edges.(get_edge_from_enum DF) <- {edge = get_edge_from_index new_edge; orientation = 0};

      let new_edge = get_edge_from_enum (edges.(get_edge_from_enum FR).edge) lxor 1 in
      edges.(get_edge_from_enum FR) <- {edge = get_edge_from_index new_edge; orientation = 0};


  method f_prime () = 
    let hold_corner = corners.(get_corner_from_enum ULF) in
      corners.(get_corner_from_enum ULF) <- corners.(get_corner_from_enum URF);
      corners.(get_corner_from_enum URF) <- corners.(get_corner_from_enum DRF);
      corners.(get_corner_from_enum DRF) <- corners.(get_corner_from_enum DLF);
      corners.(get_corner_from_enum DLF) <- hold_corner;

      let hold_edge = edges.(get_edge_from_enum UF) in
      edges.(get_edge_from_enum UF) <- edges.(get_edge_from_enum FR);
      edges.(get_edge_from_enum FR) <- edges.(get_edge_from_enum DF);
      edges.(get_edge_from_enum DF) <- edges.(get_edge_from_enum FL);
      edges.(get_edge_from_enum FL) <- hold_edge;

      self#update_corner_orientation ULF 2;
      self#update_corner_orientation URF 1;
      self#update_corner_orientation DRF 2;
      self#update_corner_orientation DLF 1;

      let new_edge = get_edge_from_enum (edges.(get_edge_from_enum UF).edge) lxor 1 in
      edges.(get_edge_from_enum UF) <- {edge = get_edge_from_index new_edge; orientation = 0};

      let new_edge = get_edge_from_enum (edges.(get_edge_from_enum FL).edge) lxor 1 in
      edges.(get_edge_from_enum FL) <- {edge = get_edge_from_index new_edge; orientation = 0};

      let new_edge = get_edge_from_enum (edges.(get_edge_from_enum DF).edge) lxor 1 in
      edges.(get_edge_from_enum DF) <- {edge = get_edge_from_index new_edge; orientation = 0};

      let new_edge = get_edge_from_enum (edges.(get_edge_from_enum FR).edge) lxor 1 in
      edges.(get_edge_from_enum FR) <- {edge = get_edge_from_index new_edge; orientation = 0};


    method f_2 () = 
      let hold_corner = corners.(get_corner_from_enum ULF) in
      corners.(get_corner_from_enum ULF) <- corners.(get_corner_from_enum DRF);
      corners.(get_corner_from_enum DRF) <- hold_corner;

      let hold_corner = corners.(get_corner_from_enum URF) in
      corners.(get_corner_from_enum URF) <- corners.(get_corner_from_enum DLF);
      corners.(get_corner_from_enum DLF) <- hold_corner;

      let hold_edge = edges.(get_edge_from_enum UF) in
      edges.(get_edge_from_enum UF) <- edges.(get_edge_from_enum DF);
      edges.(get_edge_from_enum DF) <- hold_edge;

      let hold_edge = edges.(get_edge_from_enum FL) in
      edges.(get_edge_from_enum FL) <- edges.(get_edge_from_enum FR);
      edges.(get_edge_from_enum FR) <- hold_edge;


    method r () = 
      let hold_corner = corners.(get_corner_from_enum DRB) in
      corners.(get_corner_from_enum DRB) <- corners.(get_corner_from_enum URB);
      corners.(get_corner_from_enum URB) <- corners.(get_corner_from_enum URF);
      corners.(get_corner_from_enum URF) <- corners.(get_corner_from_enum DRF);
      corners.(get_corner_from_enum DRF) <- hold_corner;

      let hold_edge = edges.(get_edge_from_enum BR) in
      edges.(get_edge_from_enum BR) <- edges.(get_edge_from_enum UR);
      edges.(get_edge_from_enum UR) <- edges.(get_edge_from_enum FR);
      edges.(get_edge_from_enum FR) <- edges.(get_edge_from_enum DR);
      edges.(get_edge_from_enum DR) <- hold_edge;

      self#update_corner_orientation DRB 2;
      self#update_corner_orientation DRF 1;
      self#update_corner_orientation URF 2;
      self#update_corner_orientation URB 1;

    
    method r_prime () = 
      let hold_corner = corners.(get_corner_from_enum DRB) in
      corners.(get_corner_from_enum DRB) <- corners.(get_corner_from_enum DRF);
      corners.(get_corner_from_enum DRF) <- corners.(get_corner_from_enum URF);
      corners.(get_corner_from_enum URF) <- corners.(get_corner_from_enum URB);
      corners.(get_corner_from_enum URB) <- hold_corner;

      let hold_edge = edges.(get_edge_from_enum BR) in
      edges.(get_edge_from_enum BR) <- edges.(get_edge_from_enum DR);
      edges.(get_edge_from_enum DR) <- edges.(get_edge_from_enum FR);
      edges.(get_edge_from_enum FR) <- edges.(get_edge_from_enum UR);
      edges.(get_edge_from_enum UR) <- hold_edge;

      self#update_corner_orientation DRB 2;
      self#update_corner_orientation DRF 1;
      self#update_corner_orientation URF 2;
      self#update_corner_orientation URB 1;


    method r_2 () =

      let hold_corner = corners.(get_corner_from_enum DRB) in
      corners.(get_corner_from_enum DRB) <- corners.(get_corner_from_enum URF);
      corners.(get_corner_from_enum URF) <- hold_corner;

      let hold_corner = corners.(get_corner_from_enum URB) in
      corners.(get_corner_from_enum URB) <- corners.(get_corner_from_enum DRF);
      corners.(get_corner_from_enum DRF) <- hold_corner;

      let hold_edge = edges.(get_edge_from_enum BR) in
      edges.(get_edge_from_enum BR) <- edges.(get_edge_from_enum FR);
      edges.(get_edge_from_enum FR) <- hold_edge;

      let hold_edge = edges.(get_edge_from_enum UR) in
      edges.(get_edge_from_enum UR) <- edges.(get_edge_from_enum DR);
      edges.(get_edge_from_enum DR) <- hold_edge;

    
    method b () = 
      let hold_corner = corners.(get_corner_from_enum ULB) in
      corners.(get_corner_from_enum ULB) <- corners.(get_corner_from_enum URB);
      corners.(get_corner_from_enum URB) <- corners.(get_corner_from_enum DRB);
      corners.(get_corner_from_enum DRB) <- corners.(get_corner_from_enum DLB);
      corners.(get_corner_from_enum DLB) <- hold_corner;

      let hold_edge = edges.(get_edge_from_enum UB) in
      edges.(get_edge_from_enum UB) <- edges.(get_edge_from_enum BR);
      edges.(get_edge_from_enum BR) <- edges.(get_edge_from_enum DB);
      edges.(get_edge_from_enum DB) <- edges.(get_edge_from_enum BL);
      edges.(get_edge_from_enum BL) <- hold_edge;

      self#update_corner_orientation ULB 1;
      self#update_corner_orientation URB 2;
      self#update_corner_orientation DRB 1;
      self#update_corner_orientation DLB 2;

      let new_edge = get_edge_from_enum (edges.(get_edge_from_enum UB).edge) lxor 1 in
      edges.(get_edge_from_enum UB) <- {edge = get_edge_from_index new_edge; orientation = 0};

      let new_edge = get_edge_from_enum (edges.(get_edge_from_enum BL).edge) lxor 1 in
      edges.(get_edge_from_enum BL) <- {edge = get_edge_from_index new_edge; orientation = 0};

      let new_edge = get_edge_from_enum (edges.(get_edge_from_enum DB).edge) lxor 1 in
      edges.(get_edge_from_enum DB) <- {edge = get_edge_from_index new_edge; orientation = 0};

      let new_edge = get_edge_from_enum (edges.(get_edge_from_enum BR).edge) lxor 1 in
      edges.(get_edge_from_enum BR) <- {edge = get_edge_from_index new_edge; orientation = 0};


    method b_prime () =
      let hold_corner = corners.(get_corner_from_enum ULB) in
      corners.(get_corner_from_enum ULB) <- corners.(get_corner_from_enum DLB);
      corners.(get_corner_from_enum DLB) <- corners.(get_corner_from_enum DRB);
      corners.(get_corner_from_enum DRB) <- corners.(get_corner_from_enum URB);
      corners.(get_corner_from_enum URB) <- hold_corner;

      let hold_edge = edges.(get_edge_from_enum UB) in
      edges.(get_edge_from_enum UB) <- edges.(get_edge_from_enum BL);
      edges.(get_edge_from_enum BL) <- edges.(get_edge_from_enum DB);
      edges.(get_edge_from_enum DB) <- edges.(get_edge_from_enum BR);
      edges.(get_edge_from_enum BR) <- hold_edge;

      self#update_corner_orientation ULB 1;
      self#update_corner_orientation URB 2;
      self#update_corner_orientation DRB 1;
      self#update_corner_orientation DLB 2;

      let new_edge = get_edge_from_enum (edges.(get_edge_from_enum UB).edge) lxor 1 in
      edges.(get_edge_from_enum UB) <- {edge = get_edge_from_index new_edge; orientation = 0};

      let new_edge = get_edge_from_enum (edges.(get_edge_from_enum BL).edge) lxor 1 in
      edges.(get_edge_from_enum BL) <- {edge = get_edge_from_index new_edge; orientation = 0};

      let new_edge = get_edge_from_enum (edges.(get_edge_from_enum DB).edge) lxor 1 in
      edges.(get_edge_from_enum DB) <- {edge = get_edge_from_index new_edge; orientation = 0};

      let new_edge = get_edge_from_enum (edges.(get_edge_from_enum BR).edge) lxor 1 in
      edges.(get_edge_from_enum BR) <- {edge = get_edge_from_index new_edge; orientation = 0};


    method b2 () =
      let hold_corner = corners.(get_corner_from_enum ULB) in
      corners.(get_corner_from_enum ULB) <- corners.(get_corner_from_enum DRB);
      corners.(get_corner_from_enum DRB) <- hold_corner;

      let hold_corner = corners.(get_corner_from_enum URB) in
      corners.(get_corner_from_enum URB) <- corners.(get_corner_from_enum DLB);
      corners.(get_corner_from_enum DLB) <- hold_corner;

      let hold_edge = edges.(get_edge_from_enum UB) in
      edges.(get_edge_from_enum UB) <- edges.(get_edge_from_enum DB);
      edges.(get_edge_from_enum DB) <- hold_edge;

      let hold_edge = edges.(get_edge_from_enum BL) in
      edges.(get_edge_from_enum BL) <- edges.(get_edge_from_enum BR);
      edges.(get_edge_from_enum BR) <- hold_edge;


    method d () = 
      let hold_corner = corners.(get_corner_from_enum DLB) in
      corners.(get_corner_from_enum DLB) <- corners.(get_corner_from_enum DRB);
      corners.(get_corner_from_enum DRB) <- corners.(get_corner_from_enum DRF);
      corners.(get_corner_from_enum DRF) <- corners.(get_corner_from_enum DLF);
      corners.(get_corner_from_enum DLF) <- hold_corner;

      let hold_edge = edges.(get_edge_from_enum DB) in
      edges.(get_edge_from_enum DB) <- edges.(get_edge_from_enum DR);
      edges.(get_edge_from_enum DR) <- edges.(get_edge_from_enum DF);
      edges.(get_edge_from_enum DF) <- edges.(get_edge_from_enum DL);
      edges.(get_edge_from_enum DL) <- hold_edge;

    
    method d_prime () = 
      let hold_corner = corners.(get_corner_from_enum DLF) in
      corners.(get_corner_from_enum DLF) <- corners.(get_corner_from_enum DRF);
      corners.(get_corner_from_enum DRF) <- corners.(get_corner_from_enum DRB);
      corners.(get_corner_from_enum DRB) <- corners.(get_corner_from_enum DLB);
      corners.(get_corner_from_enum DLB) <- hold_corner;

      let hold_edge = edges.(get_edge_from_enum DL) in
      edges.(get_edge_from_enum DL) <- edges.(get_edge_from_enum DF);
      edges.(get_edge_from_enum DF) <- edges.(get_edge_from_enum DR);
      edges.(get_edge_from_enum DR) <- edges.(get_edge_from_enum DB);
      edges.(get_edge_from_enum DB) <- hold_edge;


    method d_2 () = 
      let hold_corner = corners.(get_corner_from_enum DLB) in
      corners.(get_corner_from_enum DLB) <- corners.(get_corner_from_enum DRF);
      corners.(get_corner_from_enum DRF) <- hold_corner;

      let hold_corner = corners.(get_corner_from_enum DRB) in
      corners.(get_corner_from_enum DRB) <- corners.(get_corner_from_enum DLF);
      corners.(get_corner_from_enum DLF) <- hold_corner;

      let hold_edge = edges.(get_edge_from_enum DB) in
      edges.(get_edge_from_enum DB) <- edges.(get_edge_from_enum DF);
      edges.(get_edge_from_enum DF) <- hold_edge;

      let hold_edge = edges.(get_edge_from_enum DR) in
      edges.(get_edge_from_enum DR) <- edges.(get_edge_from_enum DL);
      edges.(get_edge_from_enum DL) <- hold_edge;

    method show_cube () = 
      for face = 0 to 5 do 
        (* print_int (int_of_face face) ; *)
        for row = 0 to 2 do
          for col = 0 to 2 do 
            print_string (string_of_color (self#get_facette_color (face_from_int face) row col)) ;
            print_string " " ;
          done ;
        done;
      done;

  end;;
