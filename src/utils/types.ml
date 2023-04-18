type color = RED | BLUE | WHITE | GREEN | YELLOW | ORANGE;;
type face = UP | LEFT | FRONT | RIGHT | BACK | DOWN;;

type move = 
    None |
    U | UPRIME | U2 | 
    D | DPRIME | D2 | 
    F | FPRIME | F2 | 
    B | BPRIME | B2 | 
    L | LPRIME | L2 | 
    R | RPRIME | R2;;

type corner_enum = 
    ULB | URB | URF | ULF | DLF | DLB | DRB | DRF;;

type edge_enum =
    UB | UR | UF | UL | FR | FL | BL | BR | DF | DL | DB | DR;;

type corner = {mutable c_index: int; mutable orientation: int};;
type edge = {mutable e_index: int; mutable orientation: int};;
type center = {mutable color: color};;

type database = {
    mutable size: int;
    mutable data: char array;
};;

type group = 
    Group_G0_G1 of database | 
    Group_G1_G2 of database | 
    Group_G2_G3 of database | 
    Group_G3_G4 of database;;  