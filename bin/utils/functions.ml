open Types
(* open Num *)

let find_edge_index edges edge_enum = 
  let rec find_edge_index_helper edge_enum index = 
    if index >= Array.length edges then failwith "Value not found in array"
    else if edges.(index).edge = edge_enum then index
    else find_edge_index_helper edge_enum (index + 1)
  in
  find_edge_index_helper edge_enum 0;;

let find_edge_index_from_edge edges edge = 
  find_edge_index edges edge.edge;;


let find_corner_index corners corner_enum = 
  let rec find_corner_index_helper corner_enum index = 
    if index >= Array.length corners then failwith "Value not found in array"
    else if corners.(index).corner = corner_enum then index
    else find_corner_index_helper corner_enum (index + 1)
  in
  find_corner_index_helper corner_enum 0;;


let pow a b = 
  let rec pow_helper a b acc = 
    if b = 0 then acc
    else pow_helper a (b - 1) (acc * a)
  in
  pow_helper a b 1;;

let factorial n =
  let rec factorial_rt n acc = 
    if n < 0 then failwith "n must be positiv"
    else if 0 = n then acc
    else factorial_rt (n-1) (n * acc)
  in factorial_rt n 1 ;;

let combinaison n k = 
  if n < k then 0
  else (factorial n) / (factorial (n - k) * (factorial k));;


let combinaison_indexer combinaisons n k = 
  let choises = Array.make_matrix n k 0 in 
  for i = 0 to n do
    for j = 0 to k do
      choises.(i).(j) <- combinaison i j
    done
  done;
  let rank = ref choises.(n).(k) in
  Array.iter (fun comb -> 
      rank := !rank - choises.(n - (comb + 1)).(k - 1);
  ) combinaisons;
  !rank - 1;;

