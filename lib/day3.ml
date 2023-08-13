module CharSet = Set.Make(Char);;

let messed_items = CharSet.inter;;

let priority_of_item item =
  match item with
  | 'a'..'z' -> int_of_char item - 96
  | 'A'..'Z' -> int_of_char item - 38
  | _ -> raise (Invalid_argument "invalid item value")
;;

let charset_of_string s = CharSet.of_seq (String.to_seq s)

let process1 input_file =
  let rucksacks = input_file
    |> Common.read_file_lines 
    |> List.map (fun line ->
      let half_len = (String.length line / 2) in
      charset_of_string (String.sub line 0 half_len), charset_of_string (String.sub line half_len half_len))
  in
  let messed_items = rucksacks 
    |> List.map 
      (fun rucksack -> 
        let (first_half, second_half) = rucksack in messed_items first_half second_half 
        |> CharSet.to_seq
        |> List.of_seq)
    |> List.flatten
  in messed_items
    |> List.map priority_of_item
    |> List.fold_left Int.add 0 
;;

let process2 input_file =
  let rucksacks = input_file
    |> Common.read_file_lines
    |> List.map charset_of_string
  in
  let rec groups_of_3 acc l = match l with
    | a::b::c::tail -> groups_of_3 ((a, b, c) :: acc) tail
    | [] -> acc
    | _ -> raise (Invalid_argument "Invalid file format")
  in rucksacks
    |> groups_of_3 []
    |> List.map
      (fun group -> let (a, b, c) = group in a
      |> CharSet.inter b
      |> CharSet.inter c
      |> CharSet.to_seq
      |> List.of_seq)
    |> List.flatten
    |> List.map priority_of_item
    |> List.fold_left Int.add 0
;;
