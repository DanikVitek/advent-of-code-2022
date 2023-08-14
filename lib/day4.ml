module Range = struct
  type 'a range = {first: 'a; last: 'a};;
  type 'a t = 'a range;;

  let make first last =
    if first <= last then Ok {first; last}
    else Error "`first` must be <= `last`"
  ;;

  let contains this that = match this, that with
    | {first=this_first; last=this_last},
      {first=that_first; last=that_last} ->
        this_first <= that_first && that_last <= this_last
  ;;

  let ( >>= ) = contains;;

  let overlap this that = match this, that with
    | {first=this_first; last=this_last},
      {first=that_first; last=that_last} ->
        not (this_last < that_first || that_last < this_first)
  ;;

  let to_string formatter range =
    let {first; last} = range in
    Printf.sprintf "%s..%s" (formatter first) (formatter last)
  ;;
end

let ( >>= ) = Range.(>>=);;

let range_pairs input_file =
  let lines = input_file |> Common.read_file_lines in
  let pairs = lines |> Seq.map (String.split_on_char ',') in 
  pairs 
    |> Seq.map (List.map (String.split_on_char '-'))
    |> Seq.map (List.map (List.map int_of_string))
    |> Seq.map (fun pair -> match pair with
      | [[ff; fl]; [sf; sl]] -> 
        Result.get_ok (Range.make ff fl), Result.get_ok (Range.make sf sl)
      | _ -> raise (Invalid_argument "Invalid file format"))
;;

let process1 input_file = input_file
  |> range_pairs
  |> Seq.filter (fun (first, second) -> first >>= second || second >>= first)
  |> Seq.fold_left (fun acc _ -> acc + 1) 0
;;

let process2 input_file =
  let overlapping = input_file
    |> range_pairs
    |> Seq.filter (fun (first, second) -> first |> Range.overlap second)
  in overlapping |> Seq.fold_left (fun acc _ -> acc + 1) 0
;;