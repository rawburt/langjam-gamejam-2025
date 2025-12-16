let debug = ref false
let lib_dir = ref "lib"
let trace msg = if !debug then Printf.printf "%s\n" msg

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
