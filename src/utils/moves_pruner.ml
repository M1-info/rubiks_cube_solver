open Types

let simplify_moves = function
  | L, L -> L2
  | R, R -> R2
  | D, D -> D2
  | U, U -> U2
  | B, B -> B2
  | F, F -> F2

  | L, LPRIME | LPRIME, L -> None
  | R, RPRIME | RPRIME, R -> None
  | D, DPRIME | DPRIME, D -> None
  | U, UPRIME | UPRIME, U -> None
  | B, BPRIME | BPRIME, B -> None
  | F, FPRIME | FPRIME, F -> None

  | L2, L2 -> None
  | R2, R2 -> None
  | D2, D2 -> None
  | U2, U2 -> None
  | B2, B2 -> None
  | F2, F2 -> None

  | L2, L | L, L2 -> LPRIME
  | R2, R | R, R2 -> RPRIME
  | D2, D | D, D2 -> DPRIME
  | U2, U | U, U2 -> UPRIME
  | B2, B | B, B2 -> BPRIME
  | F2, F | F, F2 -> FPRIME

  | LPRIME, LPRIME -> L2
  | RPRIME, RPRIME -> R2
  | DPRIME, DPRIME -> D2
  | UPRIME, UPRIME -> U2
  | BPRIME, BPRIME -> B2
  | FPRIME, FPRIME -> F2

  | L2, LPRIME | LPRIME, L2 -> L
  | R2, RPRIME | RPRIME, R2 -> R
  | D2, DPRIME | DPRIME, D2 -> D
  | U2, UPRIME | UPRIME, U2 -> U
  | B2, BPRIME | BPRIME, B2 -> B
  | F2, FPRIME | FPRIME, F2 -> F

  | _, _ -> None;;


let prune_move move last_move = 
  match move, last_move with
   
  (* Same face twitst *)
  | L , L | L , LPRIME | L , L2 -> true
  | LPRIME , L | LPRIME , LPRIME | LPRIME , L2 -> true
  | L2 , L | L2 , LPRIME | L2 , L2 -> true
  
  | R , R | R , RPRIME | R , R2 -> true
  | RPRIME , R | RPRIME , RPRIME | RPRIME , R2 -> true
  | R2 , R | R2 , RPRIME | R2 , R2 -> true

  | D , D | D , DPRIME | D , D2 -> true
  | DPRIME , D | DPRIME , DPRIME | DPRIME , D2 -> true
  | D2 , D | D2 , DPRIME | D2 , D2 -> true

  | U , U | U , UPRIME | U , U2 -> true
  | UPRIME , U | UPRIME , UPRIME | UPRIME , U2 -> true
  | U2 , U | U2 , UPRIME | U2 , U2 -> true

  | B , B | B , BPRIME | B , B2 -> true
  | BPRIME , B | BPRIME , BPRIME | BPRIME , B2 -> true
  | B2 , B | B2 , BPRIME | B2 , B2 -> true

  | F , F | F , FPRIME | F , F2 -> true
  | FPRIME , F | FPRIME , FPRIME | FPRIME , F2 -> true
  | F2 , F | F2 , FPRIME | F2 , F2 -> true

  (* Commutatives moves *)
  | F, B | F, BPRIME | F, B2 -> true
  | FPRIME, B | FPRIME, BPRIME | FPRIME, B2 -> true
  | F2, B | F2, BPRIME | F2, B2 -> true
  
  | L, R | L, RPRIME | L, R2 -> true
  | LPRIME, R | LPRIME, RPRIME | LPRIME, R2 -> true
  | L2, R | L2, RPRIME | L2, R2 -> true

  | U, D | U, DPRIME | U, D2 -> true
  | UPRIME, D | UPRIME, DPRIME | UPRIME, D2 -> true
  | U2, D | U2, DPRIME | U2, D2 -> true

  | _, _ -> false;;




