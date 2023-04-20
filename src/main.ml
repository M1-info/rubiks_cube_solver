open Utils_module
open Thistlethwaite_module

let solve_cube () = 
  let cube = new Rubiks_cube.rubiks_cube in
  cube#init ();
  cube#scramble 200;
  print_newline ();
  print_string "Start cube:";
  cube#show_cube;
  print_string "------------------";
  print_newline ();

  let pattern_database = new Pattern_database.pattern_database in 
  pattern_database#init ();

  let moves = [| Types.All_Moves; Types.Moves_Group_1; Types.Moves_Group_2; Types.Moves_Group_3 |] in
  let goals = [| Types.Goal_1; Types.Goal_2; Types.Goal_3; Types.Goal_4 |] in
  let groups = [| Types.Group_0_1; Types.Group_1_2; Types.Group_2_3; Types.Group_3_4 |] in

  let start_time = Sys.time() in

  let rec solve_cube_aux cube founded_moves i =
    if i = 4 then founded_moves
    else (
      let goal = Goals.get_goal goals.(i) in
      let moves_store = Moves_store.get_moves moves.(i) in
      let group = groups.(i) in
      print_string "Goal ";
      print_int i;
      print_string " started !";
      print_newline ();

      let delta_time = Sys.time() in

      let moves_g = Solver.ida_star pattern_database cube group goal moves_store in
      let moves_g = Moves_pruner.simplify_list_moves moves_g in

      let moves = founded_moves@moves_g in
      cube#apply_moves moves_g;
      
      cube#show_cube;
      print_newline ();
      print_string "Goal : ";
      print_int i;
      print_string " solved in : ";
      print_int (List.length moves_g);
      print_string " moves !";
      print_newline ();
      print_string "Moves : ";
      List.iter (fun x -> print_string (Utils.string_of_move x); print_string " ") moves_g;
      print_newline ();
      print_string "Time : ";
      print_float (Sys.time() -. delta_time);
      print_string " seconds !";
      print_newline ();
      print_string "------------------";
      print_newline ();

      solve_cube_aux cube moves (i + 1);
    )
  in 
  
  let solve_moves = solve_cube_aux cube [] 0 in
  let simplified_moves = Moves_pruner.simplify_list_moves solve_moves in

  print_string "Cube solved in : ";
  print_int (List.length simplified_moves);
  print_string " moves !";
  print_newline ();
  print_string "Time : ";
  print_float (Sys.time() -. start_time);
  print_string " seconds !";
  print_newline ();
;;

solve_cube ();;


  
  
      




