open Utils
open Types

let show_cube cube = 
  let spaces = "           " in
  print_newline ();

  for row = 0 to 2 do 
    print_string spaces;
    for col = 0 to 2 do 
      print_string (string_of_color (cube#get_facette_color UP row col)) ;
      print_string "  " ;
    done;
    print_newline ();
  done;

  print_newline ();

  for row = 0 to 2 do 
    for face = 1 to 4 do
      for col = 0 to 2 do 
        print_string (string_of_color (cube#get_facette_color (face_from_int face) row col)) ;
        print_string "  " ;
      done;
      print_string "  " ;
    done;
    print_newline ();
  done;

  print_newline ();

  for row = 0 to 2 do 
    print_string spaces ;
    for col = 0 to 2 do 
      print_string (string_of_color (cube#get_facette_color DOWN row col)) ;
      print_string "  " ;
    done;
    print_newline ();
  done;;