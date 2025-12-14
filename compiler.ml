open Syntax

let compile_name = function "pset" -> "engine.pset" | n -> n

let rec compile_expr = function
  | EInt i -> string_of_int i
  | EColor color -> Printf.sprintf "\"%s\"" color
  | ECall (name, exprs) ->
      let compiled_exprs = String.concat "," (List.map compile_expr exprs) in
      Printf.sprintf "%s(%s);" (compile_name name) compiled_exprs

let compile (Program exprs) =
  let body = String.concat "\n" (List.map compile_expr exprs) in
  Printf.sprintf "function game(engine) {\n%s\n}" body
