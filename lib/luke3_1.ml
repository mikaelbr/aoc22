open Tools
module SS = Set.Make (Char)

let explode s = List.init (String.length s) (String.get s)
let map_double f (a, b) = (f a, f b)

let split_to_pairs str =
  let half_len = String.length str / 2 in
  (String.sub str 0 half_len, String.sub str half_len half_len)

let map_to_sets = map_double (explode >> SS.of_list)
let inter (a, b) = SS.inter a b
let lower_offset = 96
let upper_offset = 38

let char_to_priority = function
  | a when a >= 'a' && a <= 'z' -> Char.code a - lower_offset
  | a when a >= 'A' && a <= 'Z' -> Char.code a - upper_offset
  | _ -> 0

let to_priority_code_list =
  List.map
    (split_to_pairs >> map_to_sets >> inter >> SS.elements
   >> List.map char_to_priority)

let sum = List.fold_left ( + ) 0
let run = to_priority_code_list >> List.map sum >> sum
