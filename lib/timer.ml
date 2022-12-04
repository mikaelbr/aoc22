let time f =
  let t = Sys.time () in
  let fx = f () in
  Printf.printf "Execution time: %fs\n" (Sys.time () -. t);
  fx

let run str fn x =
  let t = Sys.time () in
  let fx = fn x in
  Printf.printf "\n\n%s: %d\n> Execution time: %fµs\n" str fx
    ((Sys.time () -. t) *. 1000.0 *. 1000.0)
