# Rubik's cube solver in Ocaml

This is a implementation of a Rubik's cube solver in Ocaml. It is based on the [Thistlethwaite algorithm](https://en.wikipedia.org/wiki/Morwen_Thistlethwaite) and uses a pattern database as a heuristic to pass along each Thistlethwaite algorithm groups.
The solver is a IDA* search implementation. It is able to solve a cube in average 30 moves and in less than 0,5 seconds. The pattern database is generated using a non-iterative IDDFS algorithm. The database are stored in the `database` folder.

## Overview

The program is composed of multiples files :

- `main.ml` contains the main function of the program. It is used to run the solver.

- `rubiks_cube.ml` contains the implementation of the Rubik's cube. It is a class that contains the state of the cube and the methods to manipulate it. The cube is stored as three arrays : one for the edges, one for the corners and the lasto one for the centers. 

- `pattern_database.ml` contains the implementation of the pattern database. It is a class that contains the databases for each group and the methods to manipulate it. Each database is a array with for each state of the cube, the number of moves to solve it. To get the right index for a state, depending on the group, the program uses multiples indexers function.

- `indexer.ml` contains the implementation of the indexers. It is a file containing multiples functions to index a state of the cube. Each function is used to index a specific group. The indexers are used to get the right index for a state in the pattern database.

- `solver.ml` contains the implementation of the IDA* and the non-iterative IDDFS algorithm.

## Dependencies

The program depends on the following libraries:

- [dune](https://github.com/ocaml/dune)
- [extlib](https://github.com/ygrek/ocaml-extlib)

Depends on your ocaml installation, you can install them with:

        opam install dune extlib


## Usage

The program is build with dune. To build it, from the root of the project, run:

        dune build

To run the solver, from the root of the project:
        
        dune exec rubiks_cube_solver