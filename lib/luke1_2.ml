open Tools

let new_max_array potential_max arr =
  let rec new_max_array_int potential_max_int = function
    | x :: xs when potential_max_int > x -> potential_max_int :: xs
    | x :: xs -> x :: new_max_array_int potential_max_int xs
    | [] -> []
  in

  List.sort compare (new_max_array_int potential_max arr)

let get_max =
  let rec get_max_int current_max prev = function
    | [] -> current_max
    | Some x :: t -> get_max_int current_max (prev + x) t
    | None :: t ->
        let new_max = new_max_array prev current_max in
        get_max_int new_max 0 t
  in
  get_max_int [ 0; 0; 0 ] 0

let run = get_max >> List.fold_left ( + ) 0
