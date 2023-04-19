type color = RED | BLUE | WHITE | GREEN | YELLOW | ORANGE;;
type face = UP | LEFT | FRONT | RIGHT | BACK | DOWN;;

type move = 
    None | NoMove |
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

type group = Group_0_1| Group_1_2| Group_2_3| Group_3_4;;  
type goals = Goal_1 | Goal_2 | Goal_3 | Goal_4;;
type moves = All_Moves | Moves_Group_1 | Moves_Group_2 | Moves_Group_3;;