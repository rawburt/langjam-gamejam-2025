open Syntax

let compile_name = function
  | "pset" -> "engine.pset"
  | "button" -> "engine.button"
  | "buttonp" -> "engine.buttonp"
  | "clear" -> "engine.clear"
  | "text" -> "engine.text"
  | "debug" -> "engine.debug"
  | n -> n

let compile_bop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Lt -> "<"
  | Gt -> ">"
  | Or -> "||"
  | And -> "&&"
  | Eq -> "==="

let rec compile_var = function
  | VName (name, _) -> name
  | VSub (var, expr, _) ->
      let estr = compile_expr expr in
      Printf.sprintf "%s[%s]" (compile_var var) estr
  | VField (var, name, _) -> Printf.sprintf "%s.%s" (compile_var var) name

and compile_expr = function
  | EBool b -> string_of_bool b
  | EInt i -> string_of_int i
  | EColor color -> Printf.sprintf "\"%s\"" color
  | EStr str -> Printf.sprintf "\"%s\"" str
  | EVar var -> compile_var var
  | ECall (var, exprs, Loc line) -> (
      let name =
        match var with
        | VName (n, _) -> n
        | _ -> failwith "compile_expr: ECall: not a VName"
      in
      let compiled_exprs = String.concat "," (List.map compile_expr exprs) in
      match name with
      | "len" -> Printf.sprintf "(%s).length" compiled_exprs
      | "debug" ->
          Printf.sprintf "%s('[line %d]: ' + %s)" (compile_name name) line
            compiled_exprs
      | "str" -> compiled_exprs
      | _ -> Printf.sprintf "%s(%s)" (compile_name name) compiled_exprs)
  | EBinary (bop, e1, e2, _) ->
      let c1 = compile_expr e1 in
      let c2 = compile_expr e2 in
      let op = compile_bop bop in
      Printf.sprintf "%s %s %s" c1 op c2
  | EList (exprs, _) ->
      let items = List.map compile_expr exprs |> String.concat ", " in
      Printf.sprintf "[%s]" items
  | ERec (_, fields, _) ->
      let compile_field (n, e) = Printf.sprintf "%s:%s" n (compile_expr e) in
      let f = List.map compile_field fields |> String.concat ", " in
      Printf.sprintf "{%s}" f

let rec compile_stmt = function
  | SVar (name, _, expr, _) ->
      Printf.sprintf "let %s = %s;" name (compile_expr expr)
  | SMutate (var, expr, _) ->
      Printf.sprintf "%s = %s;" (compile_var var) (compile_expr expr)
  | SExpr (expr, _) -> compile_expr expr
  | SIfElse (expr, block1, block2, _) ->
      let e = compile_expr expr in
      let b1 = compile_block block1 in
      let b2 = Option.map compile_block block2 in
      let b2_else =
        match b2 with Some b -> Printf.sprintf "else {\n%s\n}" b | None -> ""
      in
      Printf.sprintf "if (%s) {\n%s\n} %s" e b1 b2_else
  | SFor (name, expr1, expr2, block, _) ->
      Printf.sprintf "for (let %s = %s; %s < %s; %s += 1) {\n%s\n}" name
        (compile_expr expr1) name (compile_expr expr2) name
        (compile_block block)
  | SRet (expr, _) -> Printf.sprintf "return %s;" (compile_expr expr)

and compile_block (Block stmts) =
  String.concat "\n" (List.map compile_stmt stmts)

let compile_toplevel = function
  | TLStmt stmt -> compile_stmt stmt
  | TLDef def ->
      let params = List.map fst def.params |> String.concat ", " in
      Printf.sprintf "function %s(%s) {\n%s\n}" def.name params
        (compile_block def.body)
  | TLRec _ -> ""

let add_empty_def name toplevels =
  let compare = function TLDef def -> def.name = name | _ -> false in
  match List.find_opt compare toplevels with
  | Some _ -> toplevels
  | None ->
      TLDef { name; params = []; body = Block []; loc = Loc 0; ret = None }
      :: toplevels

let ensure_engine_defs toplevels =
  add_empty_def "update" toplevels |> add_empty_def "draw"

let compile (Program toplevels) =
  let toplevels' = ensure_engine_defs toplevels in
  let body = String.concat "\n" (List.map compile_toplevel toplevels') in
  Printf.sprintf
    "function game(engine) {\n%s\nreturn {update:update,draw:draw};\n}" body
