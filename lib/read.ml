let maybe_read_line () = try Some (read_line ()) with End_of_file -> None

let read_int_lines () =
  let rec loop acc =
    match maybe_read_line () with
    | Some line -> loop (int_of_string_opt line :: acc)
    | None -> List.rev acc
  in

  loop []

let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    !lines |> List.rev
  with End_of_file ->
    close_in chan;
    !lines |> List.rev

let read_int_file filename = read_file filename |> List.map int_of_string_opt

let read_string_lines () =
  let rec loop acc =
    match maybe_read_line () with
    | Some line -> loop (line :: acc)
    | None -> List.rev acc
  in

  loop []

let print_string_list = List.iter (Printf.printf "%s\n")
let print_int_list = List.iter (Printf.printf "%d\n")