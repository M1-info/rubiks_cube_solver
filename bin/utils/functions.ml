open Types

let check_corners_parity (corners: corner array) = 
  let nb_corners = Array.length corners in
  let rec check_corners_parity_aux parity i j = 
    if i = nb_corners then parity
    else (
      let result = if (corners.(i).orientation < corners.(j).orientation) then 1 else 0 in
      parity := !parity lxor result;
      if j = nb_corners - 1 then check_corners_parity_aux parity (i + 1) 0
      else check_corners_parity_aux parity i (j + 1)
    ) in
  let parity = check_corners_parity_aux (ref 0) 0 0 in
  parity;;


let load_file filename =
  let data = ref [] in
  let chan = open_in_bin filename in
  try
    while true do
      data := input_line chan :: !data
    done;
    assert false with End_of_file ->
      close_in chan;
      List.rev !data;;


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


let combinations n k = 
  if n < k then 0
  else (factorial n) / (factorial (n - k) * (factorial k));;


let pick n k =
  factorial(n) / factorial(n - k);;
