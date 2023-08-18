let read_file_lines file_name =
  let channel = open_in file_name in
  let read_line () =
    try
      let line = input_line channel in
      Some (line, ())
    with End_of_file ->
      close_in channel;
      None
  in Seq.unfold read_line ()

let str_split_at_char_once_opt sep s =
  String.index_opt s sep
    |> Option.map (fun i ->
      String.sub s 0 i,
      String.sub s (i + 1) (String.length s - i - 1))

exception Not_yet_implemented of string

let todo ?(msg="Not yet implemented") () = raise (Not_yet_implemented msg)

exception Unimplemented of string

let unimplemented ?(msg="Not implemented") () = raise (Unimplemented msg)

exception Unreachable of string

let unreachable ?(msg="Unreachable") () = raise (Unreachable msg)