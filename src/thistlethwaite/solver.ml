open Stdint

open Utils_module.Types
open Utils_module.Priority_queue
open Utils_module.Utils
open Utils_module.Moves_pruner

open Pattern_database
open Rubiks_cube

type node = {
    cube: rubiks_cube;
    move : move;
    depth: int;
}

type prioritized_move = {
    cube: rubiks_cube;
    move : move;
    estimated_moves: int;
}

let compare_nodes (n1: prioritized_move) (n2: prioritized_move) = 
  n1.estimated_moves > n2.estimated_moves;;

let ida_star (database: pattern_database) (rubiks_cube: rubiks_cube) group_index goal (moves_store: move array) = 

  let group = database#get_group group_index in

  let nodes_stack = Stack.create () in
  let current_node = ref {cube = rubiks_cube; move = None; depth = 0} in
  let moves = Array.make 50 None in 
  let bound = ref 0 in
  let next_bound = ref (database#get_num_moves (database#get_index group_index rubiks_cube) group) in 
  let is_solved = ref (goal rubiks_cube) in

  print_string "IDA* started with bound: ";
  print_int !next_bound;
  print_newline ();

  while (not !is_solved) do 
    if Stack.is_empty nodes_stack then (
      if !bound <> 0 then (
        print_string "IDA* increased bound to: ";
        print_int !next_bound;
        print_newline ();
      );

      Stack.push {cube = rubiks_cube; move = None; depth = 0} nodes_stack;

      if !next_bound = 0xFF then failwith "Error: next bound is not set";
      if !next_bound = 0 then failwith "Error: next bound is 0";

      bound := !next_bound;
      next_bound := 0xFF;
    );
      
    current_node := Stack.pop nodes_stack;
      
    if (!current_node).depth > !bound then failwith "Error: current node depth is greater than bound";

    moves.(!current_node.depth) <- None;

    if (!current_node).depth <> 0 then (
      moves.((!current_node).depth - 1) <- !(current_node).move;
    );

    if (!current_node).depth = !bound then (
      print_string "Here";
      print_newline ();
      is_solved := goal (!current_node).cube;
    ) else (
      let successors = new priority_queue in 

      Array.iter (fun (move: move) -> 
        if((!current_node).depth = 0 || not (prune_move move ((!current_node).move))) then (

          let cube_copy = !current_node.cube#copy () in
          cube_copy#apply_move (string_of_move move);
          
          let estimated_success_move = (!current_node).depth + 1 + (database#get_num_moves (database#get_index group_index cube_copy) group) in

          if estimated_success_move <= !bound then (
            let node = {cube = cube_copy; move = move; estimated_moves = estimated_success_move} in 
            successors#push compare_nodes node;
          ) else if (estimated_success_move < !next_bound) then (
            next_bound := estimated_success_move;
          );
        )
      ) moves_store;

      while (not (successors#is_empty ())) do

        let node = successors#pop () in
        let new_node = {cube = node.cube; move = node.move; depth = ((!current_node).depth + 1)} in
        Stack.push new_node nodes_stack;
      done;
    );
    done;
  
  let moves_list = ref [] in
  let rec get_moves i =
    if i = Array.length moves || moves.(i) = None then ()
    else (
      moves_list := moves.(i) :: !moves_list;
      get_moves (i + 1);
    );
  in get_moves 0;
  List.rev !moves_list;;


let database_indexer (database: pattern_database) group_index goal solved_cube moves_store = 
  let current_depth = ref 0 in
  let index_count = ref 0 in
  let nodes_stack = Stack.create () in
  let current_node = ref {cube = solved_cube; move = None; depth = 0} in

  let group = database#get_group group_index in
  let root_index = database#get_index group_index solved_cube in
  let _ = database#set_num_moves group root_index 0 in
  index_count := !index_count + 1;

  while not (goal solved_cube) do 
    if Stack.is_empty nodes_stack then (
      current_depth := !current_depth + 1;
      Stack.push {cube = solved_cube; move = None; depth = 0} nodes_stack;
    );

    current_node := Stack.pop nodes_stack;

    Array.iter (fun (move: move) -> 
      
      if !current_node.depth = 0 || not (prune_move move ((!current_node).move)) then (
        let cube_copy = !current_node.cube#copy () in
        let cube_depth_copy = (!current_node.depth + 1) in
        
        cube_copy#apply_move (string_of_move move);
        let index = database#get_index group_index cube_copy in

        if (database#get_num_moves index group) < cube_depth_copy then (
          
        ) else (
          if cube_depth_copy = !current_depth then (
            if database#set_num_moves group root_index 0 then (
              index_count := !index_count + 1;
            );
          ) else (
            let new_node = {cube = cube_copy; move = move; depth = cube_depth_copy} in
            Stack.push new_node nodes_stack;
          )
        )
      );
      
    ) moves_store;

  done;