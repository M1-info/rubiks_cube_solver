open Utils_module

open Pattern_database
open Rubiks_cube

type node = {
    cube: rubiks_cube;
    move : Types.move;
    depth: int;
}

type prioritized_move = {
    cube: rubiks_cube;
    move : Types.move;
    estimated_moves: int;
}

let compare_nodes (n1: prioritized_move) (n2: prioritized_move) = 
  n1.estimated_moves > n2.estimated_moves;;


let moves_array_to_list moves =
  let rec get_moves list i =
    if i = Array.length moves || moves.(i) = Types.NoMove then list
    else get_moves (moves.(i) :: list;) (i + 1);
  in List.rev (get_moves [] 0);
;;


let ida_star (database: pattern_database) (rubiks_cube: rubiks_cube) (group_index: Types.group) (goal: rubiks_cube -> bool) (moves_store: Types.move array) = 
  let group = database#get_group group_index in
  let moves_length = Array.length moves_store in

  (* Get all nodes that give a lower or equal value of the current bound *)
  let get_successors current_node current_bound next_bound = 
    let rec get_successors_aux queue current_bound next_bound i = 
      if i = moves_length then (queue, next_bound) 
      else if current_node.depth = 0 || not (Moves_pruner.prune_move moves_store.(i) current_node.move) then (
        let cube_copy = current_node.cube#copy () in
        cube_copy#apply_move (Utils.string_of_move moves_store.(i));

        let estimated_success_move = current_node.depth + 1 + database#get_num_moves (database#get_index group_index cube_copy) group in

        if estimated_success_move <= current_bound then (
          let node = {cube = cube_copy; move = moves_store.(i); estimated_moves = estimated_success_move} in 
          queue#push compare_nodes node;
          get_successors_aux queue current_bound next_bound (i + 1)
        ) else if estimated_success_move < next_bound then (
          get_successors_aux queue current_bound estimated_success_move (i + 1)
        ) else(
          get_successors_aux queue current_bound next_bound (i + 1)
        )
      ) else get_successors_aux queue current_bound next_bound (i + 1)
    in 
    get_successors_aux (new Priority_queue.priority_queue) current_bound next_bound 0 
  in

  let rec ida_star_aux node_stack bound next_bound (moves: Types.move array) = 

    (* If the stack is empty we restart with another sub tree from the node root 
       and increase the estimated needed mouvements 
    *)
    if Stack.is_empty node_stack then (
      if bound <> 0 then (
        print_string "IDA* increased bound to: ";
        print_int next_bound;
        print_newline ();
      );
      Stack.push {cube = rubiks_cube; move = Types.NoMove; depth = 0} node_stack;
      ida_star_aux node_stack next_bound 0xFF moves;
    ) else (

      let current_node = Stack.pop node_stack in

      moves.(current_node.depth) <- Types.NoMove;
      if current_node.depth <> 0 then moves.(current_node.depth - 1) <- current_node.move;
  
      if current_node.depth = bound then (
        if goal current_node.cube then moves
        else ida_star_aux node_stack bound next_bound moves
      ) else (
        let (successors, new_next_bound) = get_successors current_node bound next_bound in    
        while (not (successors#is_empty ())) do
          let node = successors#pop ()  in
          let new_node = {cube = node.cube; move = node.move; depth = current_node.depth + 1} in
          Stack.push new_node node_stack;
        done;
        ida_star_aux node_stack bound new_next_bound moves;
      )
    )

  in

  let node_stack = Stack.create () in
  let bound = 0 in
  let next_bound = database#get_num_moves (database#get_index group_index rubiks_cube) group in 
  let moves = Array.make 50 Types.NoMove in
  print_string "IDA* started with bound: ";
  print_int next_bound;
  print_newline ();
  if goal rubiks_cube then []
  else moves_array_to_list (ida_star_aux node_stack bound next_bound moves)
;;


let iddfs (database: pattern_database) (solved_cube: rubiks_cube) (group_index: Types.group) (moves_store: Types.move array) = 

  let index_count = ref 0 in
  let group = database#get_group group_index in
  let root_index = database#get_index group_index solved_cube in
  let _ = database#set_num_moves root_index group 0 in
  index_count := !index_count + 1;

  let current_depth = 0 in
  let nodes_stack = Stack.create () in

  let rec iddsf_aux nodes_stack current_depth = 
    if database#is_full group_index then true
    else (
      if Stack.is_empty nodes_stack then (
        Stack.push {cube = solved_cube; move = None; depth = 0} nodes_stack;
        iddsf_aux nodes_stack (current_depth + 1);
      ) else (
        let current_node = Stack.pop nodes_stack in

        Array.iter (fun (move: Types.move) -> 
          if current_node.depth = 0 || not (Moves_pruner.prune_move move current_node.move) then (
            let cube_copy = current_node.cube#copy () in
            let cube_depth_copy = current_node.depth + 1 in
            
            cube_copy#apply_move (Utils.string_of_move move);
            let index = database#get_index group_index cube_copy in
    
            if (database#get_num_moves index group) < cube_depth_copy then () 
            else (
              if cube_depth_copy = current_depth then (
                if database#set_num_moves index group 0 then (
                  index_count := !index_count + 1;
                );
              ) else (
                let new_node = {cube = cube_copy; move = move; depth = cube_depth_copy} in
                Stack.push new_node nodes_stack;
              )
            )
          );
        ) moves_store;
        iddsf_aux nodes_stack current_depth;
      )
    )
  in iddsf_aux nodes_stack current_depth
;;
