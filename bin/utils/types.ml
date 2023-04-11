type color = RED | BLUE | WHITE | GREEN | YELLOW | ORANGE
type face = UP | LEFT | FRONT | RIGHT | BACK | DOWN

type move = 
    U | UPRIME | U2 | 
    D | DPRIME | D2 | 
    F | FPRIME | F2 | 
    B | BPRIME | B2 | 
    L | LPRIME | L2 | 
    R | RPRIME | R2

type corners = 
    ULB | URB | URF | ULF | DLF | DLB | DRB | DRF

type edges =
    UB | UR | UF | UL | FR | FL | BL | BR | DF | DL | DB | DR

type edge = {edge: edges; mutable orientation: int}
type corner =  {corner: corners; mutable orientation: int}
type center = {mutable color: color}

(* type cube = {edges: edge array; corners: corner array; centers: center array} *)