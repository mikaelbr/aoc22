open Tools

let maybe_read_line () = try Some (read_line ()) with End_of_file -> None

type player = Rock | Paper | Scissors
type fightResult = Win | Loss | Draw
type fight = player option * fightResult option

let player_of_first = function
  | "A" -> Some Rock
  | "B" -> Some Paper
  | "C" -> Some Scissors
  | _ -> None

let result_of_second = function
  | "X" -> Some Loss
  | "Y" -> Some Draw
  | "Z" -> Some Win
  | _ -> None

let players_of_line line : fight =
  line |> String.split_on_char ' ' |> function
  | x :: y :: _ -> (player_of_first x, result_of_second y)
  | _ -> (None, None)

let string_of_player = function
  | Some Rock -> "Rock"
  | Some Paper -> "Paper"
  | Some Scissors -> "Scissors"
  | _ -> "None"

let to_fight_result = function
  | Some Rock, Some Draw -> Some (Draw, Rock)
  | Some Rock, Some Win -> Some (Win, Paper)
  | Some Rock, Some Loss -> Some (Loss, Scissors)
  | Some Paper, Some Draw -> Some (Draw, Paper)
  | Some Paper, Some Win -> Some (Win, Scissors)
  | Some Paper, Some Loss -> Some (Loss, Rock)
  | Some Scissors, Some Draw -> Some (Draw, Scissors)
  | Some Scissors, Some Win -> Some (Win, Rock)
  | Some Scissors, Some Loss -> Some (Loss, Paper)
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
