open Tools

let maybe_read_line () = try Some (read_line ()) with End_of_file -> None

type player = Rock | Paper | Scissors
type fight = player option * player option
type fightResult = Win | Loss | Draw

let player_of_first = function
  | "A" -> Some Rock
  | "B" -> Some Paper
  | "C" -> Some Scissors
  | _ -> None

let player_of_second = function
  | "X" -> Some Rock
  | "Y" -> Some Paper
  | "Z" -> Some Scissors
  | _ -> None

let players_of_line line : fight =
  line |> String.split_on_char ' ' |> function
  | x :: y :: _ -> (player_of_first x, player_of_second y)
  | _ -> (None, None)

let string_of_player = function
  | Some Rock -> "Rock"
  | Some Paper -> "Paper"
  | Some Scissors -> "Scissors"
  | _ -> "None"

let to_fight_result = function
  | Some Rock, Some Rock -> Some (Draw, Rock)
  | Some Rock, Some Paper -> Some (Win, Paper)
  | Some Rock, Some Scissors -> Some (Loss, Scissors)
  | Some Paper, Some Paper -> Some (Draw, Paper)
  | Some Paper, Some Rock -> Some (Loss, Rock)
  | Some Paper, Some Scissors -> Some (Win, Scissors)
  | Some Scissors, Some Scissors -> Some (Draw, Scissors)
  | Some Scissors, Some Rock -> Some (Win, Rock)
  | Some Scissors, Some Paper -> Some (Loss, Paper)
  | _, None -> None
  | None, _ -> None

let result_to_int = function Loss -> 0 | Draw -> 3 | Win -> 6
let player_to_int = function Rock -> 1 | Paper -> 2 | Scissors -> 3

let fight_result_to_int = function
  | Some (res, play) -> result_to_int res + player_to_int play
  | _ -> 0

let sum_fight_result =
  List.map to_fight_result
  >> List.map fight_result_to_int
  >> List.fold_left ( + ) 0

let run = List.map players_of_line >> sum_fight_result
