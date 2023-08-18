let string_windows size str =
  Seq.init
    (String.length str - (size - 1))
    (fun i -> String.sub str i size)
;;

let process1 input_file =
  let signal = (input_file |> Common.read_file_lines |> Seq.to_dispenser) () |> Option.get in (* there is only 1 line *)
  let windows = signal |> string_windows 4 in
  let rec search_first_different i windows =
    match windows () with
    | Seq.Nil -> None
    | Seq.Cons (s, tl) -> match s |> String.to_seq |> List.of_seq with
      | [a; b; c; d] when a <> b && a <> c && a <> d && b <> c && b <> d && c <> d -> Some (i + 4)
      | _ -> search_first_different (i + 1) tl
  in windows |> search_first_different 0
;;

module Hashset = struct
  type 'a t = ('a, unit) Hashtbl.t

  let of_seq s = Seq.forever (fun () -> ()) |> Seq.zip s |> Hashtbl.of_seq

  let length (hs: 'a t) = Hashtbl.length hs
end

let process2 input_file =
  let signal = (input_file |> Common.read_file_lines |> Seq.to_dispenser) () |> Option.get in (* there is only 1 line *)
  let windows = signal |> string_windows 14 in
  let rec search_first_different i windows =
    match windows () with
    | Seq.Nil -> None
    | Seq.Cons (s, tl) ->
      let distinct = s |> String.to_seq |> Hashset.of_seq in
      let distinct_len = Hashset.length distinct in
      if distinct_len == String.length s then Some (i + 14)
      else search_first_different (i + 1) tl
  in search_first_different 0 windows
;;
