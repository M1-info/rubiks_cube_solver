open Stdint

type color = RED | BLUE | WHITE | GREEN | YELLOW | ORANGE
type face = UP | LEFT | FRONT | RIGHT | BACK | DOWN

type move = 
    None |
    U | UPRIME | U2 | 
    D | DPRIME | D2 | 
    F | FPRIME | F2 | 
    B | BPRIME | B2 | 
    L | LPRIME | L2 | 
    R | RPRIME | R2

type corner_enum = 
    ULB | URB | URF | ULF | DLF | DLB | DRB | DRF

type edge_enum =
    UB | UR | UF | UL | FR | FL | BL | BR | DF | DL | DB | DR

type corner = {c_enum: corner_enum; mutable orientation: int}
type edge = {e_enum: edge_enum; mutable orientation: int}
type center = {color: color}

type cube = {
    corners: corner array;
    edges: edge array;
}

type database = {
    size: int;
    data: char array;
}
type node = {
    cube: cube;
    move : move;
    depth: Uint8.t;
}

type prioritized_move = {
    cube: cube;
    move : move;
    est_move: Uint8.t;
}

(* type 't priority_queue = 't Queue.t *)