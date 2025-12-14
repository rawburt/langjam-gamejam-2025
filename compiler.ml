open Syntax

let compile_name = function "pset" -> "engine.pset" | n -> n

let rec compile_expr = function
  | EBool b -> string_of_bool b
  | EInt i -> string_of_int i
  | EColor color -> Printf.sprintf "\"%s\"" color
  | EVar (VName name) -> name
  | ECall (VName name, exprs) ->
      let compiled_exprs = String.concat "," (List.map compile_expr exprs) in
      Printf.sprintf "%s(%s);" (compile_name name) compiled_exprs

let rec compile_stmt = function
  | SVar (name, _, expr) ->
      Printf.sprintf "let %s = %s;" name (compile_expr expr)
  | SExpr expr -> compile_expr expr
  | SIfElse (expr, block1, block2) ->
      let e = compile_expr expr in
      let b1 = compile_block block1 in
      let b2 = compile_block block2 in
      Printf.sprintf "if (%s) {\n%s\n} else {\n%s\n}" e b1 b2

and compile_block stmts = String.concat "\n" (List.map compile_stmt stmts)

let compile (Program block) =
  let body = compile_block block in
  Printf.sprintf "function game(engine) {\n%s\n}" body
