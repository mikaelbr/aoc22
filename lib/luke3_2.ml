open Tools
module SS = Set.Make (Char)

let explode s = List.init (String.length s) (String.get s)
let map_triple f (a, b, c) = (f a, f b, f c)
let map_to_set = map_triple (explode >> SS.of_list)

let rec list_take_3 = function
  | x :: y :: z :: xs -> map_to_set (x, y, z) :: list_take_3 xs
  | _ -> []

let inter3 (a, b, c) = SS.inter (SS.inter a b) c
let lower_offset = 96
let upper_offset = 38

let char_to_priority = function
  | a when a >= 'a' && a <= 'z' -> Char.code a - lower_offset
  | a when a >= 'A' && a <= 'Z' -> Char.code a - upper_offset
  | _ -> 0

let to_priority_code_list =
  list_take_3 >> List.map (inter3 >> SS.elements >> List.map char_to_priority)

let sum = List.fold_left ( + ) 0
let run = to_priority_code_list >> List.map sum >> sum
