let () =
  try let a = int_of_string Sys.argv.(1)
      and b = int_of_string Sys.argv.(2) in
      Format.printf "%d\n%!" (a + b)
  with _exn -> ()
