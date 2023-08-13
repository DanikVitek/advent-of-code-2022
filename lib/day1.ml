let elves input_file =
  let lines = Common.read_file_lines input_file in
  let cals = lines |> List.map
    (fun line -> match line with
      | "" -> None
      | _ -> Some (int_of_string line))
  in
  cals |> List.fold_left
    (fun elves cal -> match cal with
      | None -> 0 :: elves
      | Some cal -> match elves with
        | [] -> [cal]
        | h::t -> h + cal :: t)
    [] (* elves *)
;;

let process1 input_file =
  let elves = elves input_file in
  let rec max max_val vals = match vals, max_val with
    | [], _ -> max_val
    | h::t, None -> max (Some h) t
    | h::t, Some m -> max (Some (Int.max h m)) t
  in elves |> max None |> Option.get
;;

let process2 input_file =
  let top_three = input_file |> elves |> List.fold_left
    (fun t3 elf -> match t3 with
      | a, b, _ when elf > a -> (elf, a, b)
      | a, b, _ when elf > b -> (a, elf, b)
      | a, b, c when elf > c -> (a, b, elf)
      | _ -> t3)
    (Int.min_int, Int.min_int, Int.min_int)
  in match top_three with a, b, c -> a + b + c
;;