open Tools

let number_pairs =
  String.split_on_char '-' >> List.map int_of_string >> function
  | x :: s :: _ -> (x, s)
  | _ -> (0, 0)

let range_pairs =
  String.split_on_char ',' >> List.map number_pairs >> function
  | x :: s :: _ -> (x, s)
  | _ -> ((0, 0), (0, 0))

let is_overlapping = function
  | (x, y), (a, b) when x >= a && y <= b -> 1
  | (x, y), (a, b) when a >= x && b <= y -> 1
  | _ -> 0

let run = List.map (range_pairs >> is_overlapping) >> List.fold_left ( + ) 0
