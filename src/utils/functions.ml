open Types


let load_file filename =
  let chan = open_in_bin filename in
  let size = in_channel_length chan in
  let data = Array.make size '\000' in
  let rec load_file_aux index =
    if index = size then
      {data = data; size = size}
    else
      let char = input_char chan in
      data.(index) <- char;
      load_file_aux (index + 1)
  in
  let result = load_file_aux 0 in
  close_in chan;
  result
;;

let save_file filename data =
  let chan = open_out_bin filename in
  Array.iter (fun x -> output_char chan x) data;
  close_out chan
;;


let bitset_to_int bitset = 
  let enum = BitSet.enum bitset in
  Enum.map (fun x -> 1 lsl x) enum
  |> Enum.fold (fun acc x -> acc lor x) 0
;;


let index_of (value: 'a) (array: 'a array) =
  let rec index_of_aux el array index = 
    if el = array.(index) then index
    else index_of_aux el array (index + 1)
  in
  index_of_aux value array 0
;;

let pow a b = 
  let rec pow_aux a b acc = 
    if b = 0 then acc
    else pow_aux a (b - 1) (acc * a)
  in
  pow_aux a b 1
;;


let factorial n =
  let rec factorial_aux n acc = 
    if n < 0 then failwith "n must be positiv"
    else if 0 = n then acc
    else factorial_aux (n-1) (n * acc)
  in factorial_aux n 1 
;;


(* nCk with order*)
let combinations n k = 
  if n < k then 0
  else (factorial n) / (factorial (n - k) * (factorial k))
;;

(* nPk without order*)
let pick n k =
  factorial(n) / factorial(n - k)
;;


(* get the nth_bit value of int *)
let nth_bit x n = x land (1 lsl n) <> 0;;


(* create a bitset array from x (MBS order) *)
let bitarray length x = 
  let array = Array.init length (fun i -> nth_bit x (length - 1 - i)) in 
  let bitset = BitSet.create length in
  Array.iteri (fun i x -> if x = true then BitSet.set bitset (length - i)) array;
  bitset
;;
