open Types

let prune_move = function
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

  | _, _ -> None




