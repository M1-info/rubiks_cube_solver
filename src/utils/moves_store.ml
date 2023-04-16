open Types

(* All moves possibles *)
let all_moves = [| 
  U ; UPRIME ; U2 ; 
  D ; DPRIME ; D2 ; 
  F ; FPRIME ; F2 ; 
  B ; BPRIME ; B2 ; 
  L ; LPRIME ; L2 ; 
  R ; RPRIME ; R2

 |];;


 (* F and B quarter tours are omitted because they can change the orientation of corner *)
let g1_moves = [| 

  U ; UPRIME ; U2 ; 
  D ; DPRIME ; D2 ; 
  F2 ; 
  B2 ; 
  L ; LPRIME ; L2 ; 
  R ; RPRIME ; R2

 |];;

 
let g2_moves = [| 

  U ; UPRIME ; U2 ; 
  D ; DPRIME ; D2 ; 
  F2 ; 
  B2 ; 
  L2 ; 
  R2

 |];;

 
let g3_moves = [| 

  U2 ; 
  D2 ; 
  F2 ; 
  B2 ; 
  L2 ; 
  R2

 |];;

 let get_moves = function
  | 0 -> all_moves
  | 1 -> g1_moves
  | 2 -> g2_moves
  | 3 -> g3_moves
  | _ -> failwith "get_moves: invalid group";;