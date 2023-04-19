open Utils_module
open Classes_module.Solver
open Classes_module.Rubiks_cube
open Classes_module.Pattern_database
open Classes_module.Goals
open Utils_module.Utils
open Utils_module.Moves_store;;


let cube = new rubiks_cube;;
cube#init ();;

cube#scramble 100;;
print_newline ();;
print_string "Start cube:";;
print_newline ();;
cube#show_cube;;
print_string "------------------";;
print_newline ();;

let pattern_database = new pattern_database;;
pattern_database#init ();;

print_string "------------------";;
(* Group 1 *)

let goal_1 = get_goal Types.Goal_1;;
let moves_store_1 = get_moves Types.All_Moves;;

let moves_g1 = ida_star pattern_database cube Types.Group_0_1 goal_1 moves_store_1;;
List.iter (fun x -> print_string (string_of_move x); print_string " ") moves_g1;;

cube#apply_moves moves_g1;;

cube#show_cube;;

print_string "------------------";;
(* Group 2 *)

let goal_2 = get_goal Types.Goal_2;;

let moves_store_2 = get_moves Types.Moves_Group_1;;

let moves_g2 = ida_star pattern_database cube Types.Group_1_2 goal_2 moves_store_2;;
List.iter (fun x -> print_string (string_of_move x); print_string " ") moves_g2;;

cube#apply_moves moves_g2;;

cube#show_cube;;

(* Group 3 *)

print_string "------------------";;

let goal_3 = get_goal Types.Goal_3;;

let moves_store_3 = get_moves Types.Moves_Group_2;;

let moves_g3 = ida_star pattern_database cube Types.Group_2_3 goal_3 moves_store_3;;

List.iter (fun x -> print_string (string_of_move x); print_string " ") moves_g3;;

cube#apply_moves moves_g3;;

cube#show_cube;;

(* Group 4 *)

print_string "------------------";;

let goal_4 = get_goal Types.Goal_4;;

let moves_store_4 = get_moves Types.Moves_Group_3;;

let moves_g4 = ida_star pattern_database cube Types.Group_3_4 goal_4 moves_store_4;;

List.iter (fun x -> print_string (string_of_move x); print_string " ") moves_g4;;

cube#apply_moves moves_g4;;

cube#show_cube;;

(* Group 5 *)



  
  
      




