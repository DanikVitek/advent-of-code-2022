let string_windows size str =
  Seq.init
    (String.length str - (size - 1))
    (fun i -> String.sub str i size)
;;

let process1 input_file =
  let signal = input_file |> Common.read_file_lines |> List.hd in (* there is only 1 line *)
  let windows = signal |> string_windows 4 in
  let rec search_first_different i windows =
    match windows () with
      | Seq.Nil -> -1
      | Seq.Cons (s, tl) -> match s |> String.to_seq |> List.of_seq with
        | [a; b; c; d] when a != b && a != c && a != d && b != c && b != d && c != d -> i + 4
        | _ -> search_first_different (i + 1) tl
  in windows |> search_first_different 0
;;

let process2 input_file =
  let signal = input_file |> Common.read_file_lines |> List.hd in (* there is only 1 line *)
  let windows = signal |> string_windows 14 in
  let rec search_first_different i windows =
    match windows () with
      | Seq.Nil -> -1
      | Seq.Cons (s, tl) ->
        let distinct = 
          Seq.init (String.length s) (fun _ -> ())
            |> Seq.zip (s |> String.to_seq)
            |> Hashtbl.of_seq
        in let distinct_len = Hashtbl.length distinct
        in if distinct_len == String.length s then i + 14
        else search_first_different (i + 1) tl
  in search_first_different 0 windows
;;
