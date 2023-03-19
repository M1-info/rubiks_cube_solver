type mouvement = U | D | L | R | F | B |
                  U'| D'| L'| R'| F'| B' ;;

type color = R | G | B | Y | W | O ;;

class rubik_cube = 
  object
    val mutable front : color array array = Array.make_matrix 3 3 W
    val mutable back : color array array = Array.make_matrix 3 3 Y
    val mutable left : color array array = Array.make_matrix 3 3 G
    val mutable right : color array array = Array.make_matrix 3 3 B
    val mutable up : color array array = Array.make_matrix 3 3 R
    val mutable down : color array array = Array.make_matrix 3 3 O

    method init () = 
      front <- Array.make_matrix 3 3 G;
      back <- Array.make_matrix 3 3 R;
      left <- Array.make_matrix 3 3 O;
      right <- Array.make_matrix 3 3 B;
      up <- Array.make_matrix 3 3 W;
      down <- Array.make_matrix 3 3 Y

    method print_unfolded () = 
      print_newline ();
      let print_line line = 
        Array.iter (fun c -> print_string (match c with
          | R -> "R "
          | G -> "G "
          | B -> "B "
          | Y -> "Y "
          | W -> "W "
          | O -> "O ")) line; in
      Array.iter(function line -> 
        print_string "      ";
        print_line up.(line);
        print_newline ();) [|0;1;2|];
      Array.iter (fun line -> 
        print_line left.(line);
        print_line front.(line);
        print_line right.(line);
        print_line back.(line);
        print_newline ();
      ) [|0;1;2|];
      Array.iter(function line -> 
        print_string "      ";
        print_line down.(line);
        print_newline ();) [|0;1;2|];

    end;;