module Fs = struct
  type variant = File of int | Dir of (t * int lazy_t)
  and entry = {name: string; variant: variant}
  and t = entry list

  let is_file = function
    | File _ -> true
    | _ -> false

  let is_dir = function
    | Dir _ -> true
    | _ -> false

  let entry_size entry = let File size | Dir (_, lazy size) = entry.variant in size

  let size = List.fold_left (fun acc entry -> acc + entry_size entry) 0

  let dir name contents = {
    name;
    variant=Dir (contents, match contents with | [] -> Lazy.from_val 0 | _ -> lazy (size contents))
  }

  let rec get_entry path fs =
    match path with
    | [] -> Some fs
    | [root] -> if fs.name = root then Some fs else None
    | root::tl -> (match fs.variant with
      | Dir (contents, _) when fs.name == root ->
        let next = contents
          |> List.to_seq 
          |> Seq.filter_map (get_entry tl)
        in Seq.to_dispenser next ()
      | _ -> None)

  let rec add_entry path new_entry fs : t =
    match path with
    | [] ->
      if fs |> List.exists (fun entry -> is_file entry.variant = is_file new_entry.variant && entry.name = new_entry.name)
      then raise (Invalid_argument ((if new_entry.variant |> is_file then "File" else "Dir") ^ " with such name already exists"))
      else new_entry::fs
    | parent::rest ->
      let parent_dir = fs |> List.find_opt (fun entry -> is_dir entry.variant && entry.name = parent) in
      match parent_dir with
      | Some {variant=Dir (contents, _); _} ->
        dir parent (add_entry rest new_entry contents) :: (fs |> List.filter (fun entry -> entry.name <> parent || is_file entry.variant))
      | _ -> raise (Invalid_argument "Directory with such name not found")

  let path_to_string path =
    if List.length path = 0 then "/"
    else
      let buf = Buffer.create (path |> List.map String.length |> List.fold_left (+) (List.length path)) in
      path |> List.iter (fun dir -> Buffer.add_string buf ("/" ^ dir));
      buf |> Buffer.to_bytes |> String.of_bytes

  let entry_to_string = function
    | {name; variant=File size} -> Printf.sprintf "- %s (file, size=%d)" name size
    | {name; variant=Dir _} -> Printf.sprintf "- %s (dir)" name

  let to_string fs =
    let output = Buffer.create 0 in
    let rec to_string' indent entry =
      let prefix = (String.make indent ' ') in
      Buffer.add_string output (Printf.sprintf "%s%s\n" prefix (entry_to_string entry));
      match entry.variant with
      | Dir (contents, _) -> contents |> List.iter (to_string' (indent + 2))
      | _ -> ()
    in to_string' 0 {name="/"; variant=Dir fs};
    output |> Buffer.to_bytes |> String.of_bytes

  let of_terminal_output_seq lines =
    let rec aux fs lines path =
      match lines () with
      | Seq.Nil -> fs
      | Seq.Cons(line, next_command) ->
        match line with
        | "$ ls" -> aux fs next_command path
        | "$ cd /" -> aux fs next_command []
        | "$ cd .." -> aux fs next_command (List.tl path)
        | _ -> match Scanf.sscanf_opt line "$ cd %s" (fun s -> s) with
          | Some dir -> aux fs next_command (dir::path)
          | None -> match Scanf.sscanf_opt line "%u %s" (fun size name -> {name; variant=File size}) with
            | Some file -> aux (fs |> add_entry (path |> List.rev) file) next_command path
            | None -> match Scanf.sscanf_opt line "dir %s" (fun name -> dir name []) with
              | Some dir -> aux (fs |> add_entry (path |> List.rev) dir) next_command path
              | None -> raise (Invalid_argument "Invalid file format")
    in aux [] lines []
end

let construct_fs input_file =
  let terminal_output = input_file |> Common.read_file_lines in
  Fs.of_terminal_output_seq terminal_output

let process1 input_file =
  let fs = construct_fs input_file in
  let max_small_dir_size = 100_000 in
  let rec traverse acc fs =
    match fs with
    | [] -> acc
    | Fs.{variant=Dir (contents, lazy size); _}::rest ->
      rest |> traverse (contents |> traverse (if size <= max_small_dir_size then acc + size else acc))
    | _::rest -> rest |> traverse acc
  in fs |> traverse 0

let process2 input_file =
  let fs = construct_fs input_file in
  let max_fs_size = 70_000_000 in
  let required_free_space = 30_000_000 in
  let total_fs_size = Fs.size fs in
  let available_space = max_fs_size - total_fs_size in
  let min_space_to_free = max (required_free_space - available_space) 0 in
  let big_enough_dirs fs =
    let rec traverse acc fs =
      match fs with
      | [] -> acc
      | Fs.{variant=Dir (contents, lazy size); _}::rest ->
        rest |> traverse (contents |> traverse (if size >= min_space_to_free then size :: acc else acc))
      | _::rest -> rest |> traverse acc
    in traverse [] fs
  in let min_size sizes =
    let rec aux m sizes =
      match sizes with
      | [] -> m
      | size::rest -> rest |> aux (Some (Option.fold m ~some:(min size) ~none:size))
    in sizes |> aux None
  in fs |> big_enough_dirs |> min_size |> Option.value ~default:total_fs_size
