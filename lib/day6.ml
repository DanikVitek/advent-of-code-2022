let process1 input_file =
  let signal = input_file
    |> Common.read_file_lines
    |> List.hd (* there is only 1 line *)
  in 
  let windows = Seq.init
    (String.length signal - 3)
    (fun i -> String.sub signal i 4)
  in
  let rec search_first_different i windows =
    match windows () with
      | Seq.Nil -> -1
      | Seq.Cons (s, tl) -> match s |> String.to_seq |> List.of_seq with
        | [a; b; c; d] when a != b && a != c && a != d && b != c && b != d && c != d -> i + 4
        | _ -> search_first_different (i + 1) tl
  in windows |> search_first_different 0
;;