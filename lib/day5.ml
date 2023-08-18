module Move = struct
  type t = {amount: int; from_where: int; to_where: int}

  let of_string s =
    Scanf.sscanf
      s
      "move %u from %u to %u"
      (fun amount from_where to_where -> {amount; from_where; to_where})
end

module Moves = struct
  type t = Move.t Seq.t

  let of_string_seq: string Seq.t -> t = Seq.map Move.of_string
end

module Crates = struct
  type t = char Stack.t array

  let of_string_list = function
    | [] -> raise (Invalid_argument "must be at least 1 layer")
    | indices::layers ->
      let crates = indices
        |> String.trim
        |> String.to_seq
        |> Seq.filter_map (fun ch -> if ch = ' ' then None else Some (Stack.create ()))
        |> Array.of_seq
      in
      layers |> List.iter (fun layer ->
        for i = 0 to Array.length crates - 1 do
          match layer.[1 + i * 4] with
          | ' ' -> ()
          | ch -> crates.(i) |> Stack.push ch
        done
      );
      crates

  let to_string crates =
    let rec list_to_string = function
      | [] -> ""
      | [x] -> String.make 1 x
      | h::t -> String.make 1 h ^ ", " ^ list_to_string t
    in
    crates |> Array.fold_left
      (fun s stack -> s ^ "[" ^ (stack |> Stack.to_seq |> List.of_seq |> List.rev |> list_to_string) ^ "]\n")
      ""
end

let parse_instructions input_file =
  let lines = input_file |> Common.read_file_lines in
  let rec split lines crates second_half =
    if second_half then Crates.of_string_list crates, Moves.of_string_seq lines
    else match lines() with
      | Seq.Cons(h, t) when String.length h == 0 -> split t crates true
      | Seq.Cons(h, t) -> split t (h::crates) false
      | _ -> raise (Invalid_argument "Invalid file format")
  in split lines [] false

let process1 input_file =
  let (crates, moves) = input_file |> parse_instructions in
  let rec make_moves moves = match moves() with
    | Seq.Nil -> crates
      |> Array.map Stack.pop_opt
      |> Array.to_seq
      |> Seq.filter_map Fun.id
      |> String.of_seq
    | Seq.Cons(Move.{amount; from_where; to_where}, rest) ->
      for _ = amount downto 1 do
        Stack.push
          (crates.(from_where - 1) |> Stack.pop)
          crates.(to_where - 1)
      done;
      make_moves rest
  in make_moves moves

let process2 input_file =
  let (crates, moves) = input_file |> parse_instructions in
  let rec make_moves moves = match moves() with
  | Seq.Nil -> crates
    |> Array.map Stack.pop_opt
    |> Array.to_seq
    |> Seq.filter_map Fun.id
    |> String.of_seq
  | Seq.Cons(Move.{amount; from_where; to_where}, rest) ->
    let moved_crates = ref [] in
    for _ = amount downto 1 do
      moved_crates := (crates.(from_where - 1) |> Stack.pop) :: !moved_crates
    done;
    !moved_crates |> List.iter (fun c -> crates.(to_where - 1) |> Stack.push c);
    make_moves rest
  in make_moves moves
;;