(* open Classes_module.Rubiks_cube
open Classes_module.Pattern_database *)
(* open Utils_module.Prints *)
open Utils_module.Functions

let bits = BitSet.create 10;;

BitSet.set bits 0;;

BitSet.set bits 1;;

BitSet.set bits 3;;

(* let _ = print_newline ();;
let _ = print_int (BitSet.count bits);;
let _ = print_newline ();;
let enum = BitSet.enum bits;;
let _ = Enum.iter (fun x -> print_int x; print_newline ()) enum;; *)

let _ = print_newline ();;
let _ = print_int (bitset_to_int bits);; 


(* let database = new pattern_database ;;
database#init ;;

let cube = new rubiks_cube ;;
cube#init () ;;
cube#scramble 100 ;;

let _ = print_int (database#get_index_group_1 (cube#get_edges ()));; *)


(* let cube = new rubiks_cube ;; *)

(* let cube = new rubiks_cube ;;
cube#init () ;;
cube#scramble 10 ;; *)
(* show_cube cube;; *)
(* cube#b () ;;
show_cube cube;; *)

