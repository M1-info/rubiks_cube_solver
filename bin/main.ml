open Classes_module.Rubiks_cube

let cube = new rubiks_cube ;;
cube#init () ;;

let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines;
      print_string "line: ";
      print_string (input_line chan);
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;


read_file "/home/caehtel/dev/algo_complexity/rubiks_cube_solver/bin/databases/corner.pdb" ;;
