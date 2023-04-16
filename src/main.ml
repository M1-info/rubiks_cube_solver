open Classes_module.Solver
open Classes_module.Rubiks_cube
open Classes_module.Pattern_database
open Classes_module.Goals
open Utils_module.Utils
(* open Utils_module.Functions *)

open Utils_module.Moves_store

let cube = new rubiks_cube;;
cube#init ();;
cube#scramble 100;;
cube#show_cube;;


let pattern_database = new pattern_database;;
pattern_database#init ();;

let goal = get_goal 0;;
let moves_store = get_moves 0;;

(* let _ = database_indexer pattern_database 0 goal cube moves_store;;

let group = pattern_database#get_group 0;;
save_file "src/databases/test.pdb" group.data;; *)


let moves = ida_star pattern_database cube 0 goal moves_store;;

List.iter (fun x -> print_string (string_of_move x); print_string " ") moves;;

(* 
let solve_cube () =
  let cube = new rubiks_cube in
  let pattern_database = new pattern_database in
  pattern_database#init ();
  (* get user input *)
  let input = read_line () in
  print_string input; print_newline ();
  let solution_moves = ref [] in
  cube#init ();
  cube#show_cube;
  cube#scramble 100;
  let rec solve_cube_aux = function
    | 2 -> ()
    | group_index -> 
      let goal = get_goal group_index in
      let moves_store = get_moves group_index in
      let moves = Solver.ida_star pattern_database cube group_index goal moves_store in
      solution_moves := moves @ !solution_moves;
      print_int (List.length !solution_moves);
      solve_cube_aux (group_index + 1);
  in solve_cube_aux 0;
  cube#apply_moves (List.rev (!solution_moves));
  cube#show_cube;;

let _ = solve_cube ();; *)

  
  
      




