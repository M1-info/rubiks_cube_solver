open Types

let prune_move previous next = 
  match previous, next with
  | L, L -> L2
  | R, R -> R2
  | D, D -> D2
  | U, U -> U2
  | B, B -> B2
  | F, F -> F2

  | L, LPRIME -> None
  | R, RPRIME -> None
  | D, DPRIME -> None
  | U, UPRIME -> None
  | B, BPRIME -> None
  | F, FPRIME -> None

  | LPRIME, L -> None
  | RPRIME, R -> None
  | DPRIME, D -> None
  | UPRIME, U -> None
  | BPRIME, B -> None
  | FPRIME, F -> None

  | L2, L2 -> None
  | R2, R2 -> None
  | D2, D2 -> None
  | U2, U2 -> None
  | B2, B2 -> None
  | F2, F2 -> None

  | L2, L -> LPRIME
  | R2, R -> RPRIME
  | D2, D -> DPRIME
  | U2, U -> UPRIME
  | B2, B -> BPRIME
  | F2, F -> FPRIME

  | L, L2 -> LPRIME
  | R, R2 -> RPRIME
  | D, D2 -> DPRIME
  | U, U2 -> UPRIME
  | B, B2 -> BPRIME
  | F, F2 -> FPRIME

  | LPRIME, LPRIME -> L2
  | RPRIME, RPRIME -> R2
  | DPRIME, DPRIME -> D2
  | UPRIME, UPRIME -> U2
  | BPRIME, BPRIME -> B2
  | FPRIME, FPRIME -> F2

  | L2, LPRIME -> L
  | R2, RPRIME -> R
  | D2, DPRIME -> D
  | U2, UPRIME -> U
  | B2, BPRIME -> B
  | F2, FPRIME -> F

  | LPRIME, L2 -> L
  | RPRIME, R2 -> R
  | DPRIME, D2 -> D
  | UPRIME, U2 -> U
  | BPRIME, B2 -> B
  | FPRIME, F2 -> F

  | _, _ -> None




