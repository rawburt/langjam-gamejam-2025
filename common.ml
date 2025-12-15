let debug = ref false

let error ?loc ?kind msg =
  let location =
    match loc with
    | Some (Syntax.Loc (f, l)) -> Printf.sprintf "[%s:%d] " f l
    | None -> ""
  in
  let error_kind = match kind with Some k -> k ^ ": " | None -> "" in
  let header = location ^ error_kind in
  Printf.eprintf "%s%s\n" header msg;
  exit 1
