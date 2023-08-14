type move = Rock | Paper | Scissors;;

let move_of_string_opt = function
  | "A" | "X" -> Some Rock
  | "B" | "Y" -> Some Paper
  | "C" | "Z" -> Some Scissors
  | _ -> None
;;

type round = {opponent: move; me: move};;

let points_of_move = function
  | Rock -> 1
  | Paper -> 2
  | Scissors -> 3
;;

type outcome = Win | Loss | Draw;;

let outcome_of_round = function
  | {me = Rock; opponent = Paper}
  | {me = Paper; opponent = Scissors}
  | {me = Scissors; opponent = Rock} -> Loss
  | {me = Rock; opponent = Scissors} 
  | {me = Paper; opponent = Rock}
  | {me = Scissors; opponent = Paper} -> Win
  | _ -> Draw
;;

let points_of_outcome = function
  | Win -> 6
  | Loss -> 0
  | Draw -> 3
;;

let points_of_round round = points_of_move round.me + points_of_outcome (outcome_of_round round);;

let process1 input_file =
  let rounds = input_file 
    |> Common.read_file_lines 
    |> Seq.map (String.split_on_char ' ')
    |> Seq.map (fun round -> match List.map move_of_string_opt round with
      | [Some opponent; Some me] -> {opponent; me}
      | _ -> raise (Invalid_argument "File is of invalid format"))
  in rounds
    |> Seq.map points_of_round
    |> Seq.fold_left (+) 0
;;

let move_of_string_opt = function
  | "A" -> Some Rock
  | "B" -> Some Paper
  | "C" -> Some Scissors
  | _ -> None
;;

let outcome_of_string_opt = function
  | "X" -> Some Loss
  | "Y" -> Some Draw
  | "Z" -> Some Win
  | _ -> None
;;

let my_move opponent_move needed_outcome = match needed_outcome with
  | Draw -> opponent_move
  | Win -> (match opponent_move with
    | Rock -> Paper
    | Paper -> Scissors
    | Scissors -> Rock)
  | Loss -> (match opponent_move with
    | Rock -> Scissors
    | Paper -> Rock
    | Scissors -> Paper)
;;

let points_of_round (opponent_move, needed_outcome) =
  points_of_move (my_move opponent_move needed_outcome) + points_of_outcome needed_outcome
;;

let process2 input_file =
  let rounds = input_file
    |> Common.read_file_lines 
    |> Seq.map (String.split_on_char ' ')
    |> Seq.map (fun round -> match round with
      | [op; oc] -> (match move_of_string_opt op, outcome_of_string_opt oc with
        | Some opponent_move, Some needed_outcome -> opponent_move, needed_outcome
        | _ -> raise (Invalid_argument "Invalid file format"))
      | _ -> raise (Invalid_argument "Invalid file format"))
  in rounds
    |> Seq.map points_of_round
    |> Seq.fold_left (+) 0
;;