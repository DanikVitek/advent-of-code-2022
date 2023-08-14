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
    |> Seq.map (fun line ->
      let half_len = (String.length line / 2) in
      charset_of_string (String.sub line 0 half_len), charset_of_string (String.sub line half_len half_len))
  in
  let messed_items = rucksacks 
    |> Seq.map 
      (fun rucksack -> 
        let (first_half, second_half) = rucksack in messed_items first_half second_half 
        |> CharSet.to_seq)
    |> Seq.flat_map (fun s -> s)
  in messed_items
    |> Seq.map priority_of_item
    |> Seq.fold_left (+) 0
;;

let process2 input_file =
  let rucksacks = input_file
    |> Common.read_file_lines
    |> Seq.map charset_of_string
  in
  let groups_of_3 r = match r () with
    | Seq.Nil -> None
    | Seq.Cons(a, rest) -> match rest () with
      | Seq.Nil -> raise (Invalid_argument "Invalid file format")
      | Seq.Cons(b, rest) -> match rest () with
        | Seq.Nil -> raise (Invalid_argument "Invalid file format")
        | Seq.Cons(c, rest) -> Some ((a, b, c), rest)
  in rucksacks
    |> Seq.unfold groups_of_3
    |> Seq.map
      (fun group -> let (a, b, c) = group in a
      |> CharSet.inter b
      |> CharSet.inter c
      |> CharSet.to_seq)
    |> Seq.flat_map (fun s -> s)
    |> Seq.map priority_of_item
    |> Seq.fold_left (+) 0
;;
