open Renderer_module;;
open Thistlethwaite_module;;
open Utils_module;;

let solve_cube cube = 
  let cube = cube#copy () in
  print_newline ();
  print_newline ();
  print_string "Scrambled cube:";
  print_newline ();
  cube#show_cube;
  print_newline ();
  print_newline ();
  print_string "------------------";
  print_newline ();
  print_newline ();

  let pattern_database = new Pattern_database.pattern_database in 
  pattern_database#init ();

  let moves = [| Types.All_Moves; Types.Moves_Group_1; Types.Moves_Group_2; Types.Moves_Group_3 |] in
  let goals = [| Types.Goal_1; Types.Goal_2; Types.Goal_3; Types.Goal_4 |] in
  let groups = [| Types.Group_0_1; Types.Group_1_2; Types.Group_2_3; Types.Group_3_4 |] in

  let start_time = Sys.time() in

  let rec solve_cube_aux cube founded_moves founded_moves_by_group i =
    if i = 4 then (founded_moves, founded_moves_by_group)
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
      let moves_multiarray = founded_moves_by_group@[moves_g] in
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
      print_newline ();
      print_string "------------------";
      print_newline ();
      print_newline ();

      solve_cube_aux cube moves moves_multiarray (i + 1);
    )
  in 
  
  let solve_moves, solve_moves_by_group = solve_cube_aux cube [] [] 0 in
  let simplified_moves = Moves_pruner.simplify_list_moves solve_moves in

  print_string "Cube solved in : ";
  print_int (List.length simplified_moves);
  print_string " moves !";
  print_newline ();
  print_string "Time : ";
  print_float (Sys.time() -. start_time);
  print_string " seconds !";
  print_newline ();
  
  (simplified_moves, solve_moves_by_group)
;;

let cube = new Rubiks_cube.rubiks_cube;;
cube#init ();
print_newline ();
print_newline ();
print_string "Mouvement to scramble the cube : ";
print_newline ();
cube#scramble 100;;
let moves, moves_by_group = solve_cube cube;;


print_newline ();
print_string "Press enter to start the solution visualisation !";
print_newline ();

Graphics.open_graph " 800x600";;
Graphics.set_window_title "Rubik's Cube";;
Rubiks_cube_renderer.draw_cube cube;;
ignore (read_line ());;
let idx = ref 0 in
let count = ref 0 in
let moves_store = [| Types.All_Moves; Types.Moves_Group_1; Types.Moves_Group_2; Types.Moves_Group_3 |] in
List.iteri(fun i move -> 
  Graphics.clear_graph ();
  cube#apply_move (Utils.string_of_move move);
  Rubiks_cube_renderer.draw_cube cube;
  Graphics.moveto 100 (Graphics.size_y () - 50);
  Graphics.set_font "-adobe-helvetica-bold-r-*-*-14-100-*-*-*-*-*";
  Graphics.draw_string "Group ";
  Graphics.draw_string (string_of_int !idx); 
  Graphics.draw_string " - Allowed moves : <";
  Array.iteri (fun i x -> Graphics.draw_string (Utils.string_of_move x); if i <> Array.length (Moves_store.get_moves moves_store.(!idx)) - 1 then Graphics.draw_string " ";) (Moves_store.get_moves moves_store.(!idx));
  Graphics.draw_string ">";
  Graphics.set_font "-adobe-helvetica-bold-r-*-*-25-180-100-100-*-*-iso8859-1";
  Graphics.moveto (Graphics.size_x () - 200) (Graphics.size_y () - 100);
  Graphics.draw_string "Move : ";
  Graphics.draw_string (Utils.string_of_move move);
  Graphics.moveto (Graphics.size_x () - 200) (Graphics.size_y () - 150);
  Graphics.draw_string " (";
  Graphics.draw_string (string_of_int (i + 1));
  Graphics.draw_string " / ";
  Graphics.draw_string (string_of_int (List.length moves));
  Graphics.draw_string ")";
  Unix.sleepf 0.5 ;

  if !count = ((List.length (List.nth moves_by_group !idx)) - 1) then (
    idx := !idx + 1;
    idx := Stdlib.min 3 !idx;
    count := 0;
  );
  count := !count + 1;
  ) moves;;
ignore (read_line ());;
Graphics.close_graph ();;
exit 0;;






  
  
      




