open Classes_module.Rubiks_cube
open Classes_module.Pattern_database
(* open Utils_module.Prints *)

let database = new pattern_database ;;
database#init ;;

let cube = new rubiks_cube ;;
cube#init () ;;
cube#scramble 100 ;;

let _ = print_int (database#get_index_group_1 (cube#get_edges ()));;


(* let cube = new rubiks_cube ;; *)

(* let cube = new rubiks_cube ;;
cube#init () ;;
cube#scramble 10 ;; *)
(* show_cube cube;; *)
(* cube#b () ;;
show_cube cube;; *)

