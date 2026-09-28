let is_white = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false

let () =
  let rec go () = match input_line stdin with
    | line ->
      let line = String.drop_last_while is_white line in
      print_endline line; go ()
    | exception End_of_file -> () in
  go ()
