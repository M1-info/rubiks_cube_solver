open Utils_module.Types
open Pattern_database
open Rubiks_cube
open Stdint
(* ida* algorithm *)

let ida_star (database: pattern_database) (rubiks_cube: rubiks_cube) group_index goal (*moves_store*) = 

  let cube = {edges = rubiks_cube#get_edges (); corners = rubiks_cube#get_corners ()} in
  let group = database#get_group group_index in

  let nodes_stack = Stack.create () in
  let current_node = ref {cube = cube; move = None; depth = Uint8.zero} in
  let moves = Array.make 50 None in 
  let bound = ref (Uint8.zero) in
  let next_bound = ref (database#get_num_moves (database#get_index group_index rubiks_cube) group) in 

  let rec find_goal goal_finded =
    match goal_finded with 
    | true -> ()
    | false -> 
        if (nodes_stack |> Stack.length) <> 0 then (
          if !bound <> Uint8.zero then (
            (* Print *)
          );

          Stack.push {cube = cube; move = None; depth = Uint8.zero} nodes_stack;

          bound := Uint8.of_int (!next_bound);
          next_bound := 0;
          
        );

        current_node := Stack.pop nodes_stack;

        let current_depth = Uint8.to_int !(current_node).depth in
        moves.(current_depth) <- None;

        if current_depth <> 0 then (
          moves.(current_depth - 1) <- !(current_node).move;
        );

        if current_depth = Uint8.to_int !bound then (
          let is_satisfied_goal = goal rubiks_cube in 
          if is_satisfied_goal then find_goal true;
        );

        find_goal goal_finded;
  in
  find_goal false;

        (* find_goal is_solved *)
    