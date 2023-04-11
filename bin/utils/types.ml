type color = RED | GREEN | WHITE | BLUE | YELLOW | ORANGE
type face = UP | LEFT | FRONT | RIGHT | BACK | DOWN

type move = 
    U | UPRIME | U2 | 
    D | DPRIME | D2 | 
    F | FPRIME | F2 | 
    B | BPRIME | B2 | 
    L | LPRIME | L2 | 
    R | RPRIME | R2

type corners = 
    URF | ULF | ULB | URB | DRF | DLF | DLB | DRB

type edges =
    UB | UR | UF | UL | FR | FL | BL | BR | DF | DL | DB | DR

type edge = {edge: edges; mutable orientation: int}
type corner =  {corner: corners; mutable orientation: int}
type center = {mutable color: color}