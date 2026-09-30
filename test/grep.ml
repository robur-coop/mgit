let find ?(off = 0) ~sub str =
  let len = String.length sub in
  let rec go idx =
    if idx + len > String.length str then None
    else if String.sub str idx len = sub then Some idx
    else go (idx + 1) in
  go off

let is_digit = function '0' .. '9' -> true | _ -> false

let matches ~sub str =
  let len = String.length sub in
  let rec go off acc = match find ~off ~sub str with
    | None -> List.rev acc
    | Some idx ->
      let stop = ref (idx + len) in
      while !stop < String.length str && is_digit str.[!stop] do incr stop done;
      go !stop (String.sub str idx (!stop - idx) :: acc) in
  go 0 []

let fold fn acc =
  let rec go acc = match input_line stdin with
    | line -> go (fn acc line)
    | exception End_of_file -> acc in
  go acc

let () = match List.tl (Array.to_list Sys.argv) with
  | [ "-c"; sub ] ->
    let n = fold (fun n line -> if Option.is_some (find ~sub line) then n + 1 else n) 0 in
    print_endline (string_of_int n)
  | [ "-o"; sub ] ->
    fold (fun acc line -> List.rev_append (matches ~sub line) acc) []
    |> List.sort_uniq String.compare
    |> List.iter print_endline
  | subs ->
    let fn () line =
      if List.exists (fun sub -> Option.is_some (find ~sub line)) subs
      then print_endline line in
    fold fn ()
