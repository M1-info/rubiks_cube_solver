(* open Classes_module.Rubiks_cube *)
open Classes_module.Pattern_database 
(* open Stdint *)
(* open Classes_module.Solver *)

let pattern_database = new pattern_database ;;
pattern_database#init;;

let g1 = pattern_database#get_group_1 ();;

let _ = print_char g1.data.(664);;

(* let _ = print_newline ();;
let _ = print_int g1.size;;
let _ = print_newline ();;
let _ = print_int (Array.length g1.data);;

let _ = print_newline ();;
let _ = print_string "Group 1 : ";; *)

(* Array.iter (fun x -> print_string (Uint8.to_string x); print_newline ()) g1.data;; *)
(* Array.iter (fun x -> print_string (Uint8.to_string x); print_newline ()) g1.data;; *)



(* let main () = 
  let cube = new rubiks_cube in
  cube#scramble 100;

  let pattern_database = new pattern_database in
  pattern_database#init; *)

  (* let solver = new solver pattern_database in

  let is_solved = ref false in 
  let rec solve = function 
   | true -> ()
    | false ->  *)
        (* voir à quel etape on est *)


