let elves input_file =
  let lines = Common.read_file_lines input_file in
  let cals = lines |> Seq.map
    (fun line -> match line with
      | "" -> None
      | _ -> Some (int_of_string line))
  in
  let rec elf_total acc cals = match acc, cals() with
    | None, Seq.Cons(cal, tl) -> elf_total cal tl
    | Some acc, Seq.Cons(Some cal, tl) -> elf_total (Some (acc + cal)) tl
    | Some acc, Seq.Nil -> acc, Seq.empty
    | Some acc, Seq.Cons(None, tl) -> acc, tl
    | None, Seq.Nil -> raise @@ Invalid_argument "Invalid file format"
  in Seq.unfold (* ('b -> ('a * 'b) option) -> 'b -> 'a Seq.t *)
    (fun cals -> match cals() with
      | Seq.Nil -> None
      | Seq.Cons(cal, tl) -> Some(elf_total cal tl))
    cals

let process1 input_file =
  let (elves: int Seq.t) = elves input_file in
  let max_of_seq s =
    let rec max' max_val vals = match vals (), max_val with
      | Seq.Nil, _ -> max_val
      | Seq.Cons(h, t), None -> max' (Some h) t
      | Seq.Cons(h, t), Some m -> max' (Some (max h m)) t
    in s |> max' None
  in elves |> max_of_seq |> Option.get

let process2 input_file =
  let top_three = input_file |> elves |> Seq.fold_left
    (fun t3 elf -> match t3 with
      | a, b, _ when elf > a -> (elf, a, b)
      | a, b, _ when elf > b -> (a, elf, b)
      | a, b, c when elf > c -> (a, b, elf)
      | _ -> t3)
    (Int.min_int, Int.min_int, Int.min_int)
  in let a, b, c = top_three in a + b + c
