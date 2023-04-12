open Types

let find_edge_index edges edge_enum = 
  let rec find_edge_index_helper edge_enum index = 
    if index >= Array.length edges then failwith "Value not found in array"
    else if edges.(index).edge = edge_enum then index
    else find_edge_index_helper edge_enum (index + 1)
  in
  find_edge_index_helper edge_enum 0;;


let find_corner_index corners corner_enum = 
  let rec find_corner_index_helper corner_enum index = 
    if index >= Array.length corners then failwith "Value not found in array"
    else if corners.(index).corner = corner_enum then index
    else find_corner_index_helper corner_enum (index + 1)
  in
  find_corner_index_helper corner_enum 0;;