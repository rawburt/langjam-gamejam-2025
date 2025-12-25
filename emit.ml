open Syntax

type env = { ffi_map : (string * string) list }

let build_ffi_map toplevels =
  let add_ffi map = function
    | TLDef def -> (
        match def.ffi with
        | Some template -> (def.name, template) :: map
        | None -> map)
    | _ -> map
  in
  List.fold_left add_ffi [] toplevels

let emit_name = function
  | "pset" -> "engine.pset"
  | "button" -> "engine.button"
  | "buttonp" -> "engine.buttonp"
  | "clear" -> "engine.clear"
  | "text" -> "engine.text"
  | "debug" -> "engine.debug"
  | "render" -> "engine.render"
  | "render_overlay" -> "engine.renderOverlay"
  | "delete" -> "list_delete"
  | n -> n

let emit_bop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Lt -> "<"
  | Gt -> ">"
  | Lte -> "<="
  | Gte -> ">="
  | Or -> "||"
  | And -> "&&"
  | Eq -> "==="
  | Neq -> "!=="

let emit_uop = function Minus -> "-" | Negate -> "!"

let rec emit_var env = function
  | VName (name, _) -> name
  | VSub (var, expr, _) ->
      let estr = emit_expr env expr in
      Printf.sprintf "%s[%s]" (emit_var env var) estr
  | VField (var, name, _) -> Printf.sprintf "%s.%s" (emit_var env var) name

and emit_const = function
  | CNull -> "null"
  | CInt i -> string_of_int i
  | CBool b -> if b then "true" else "false"
  | CStr s -> Printf.sprintf "'%s'" s
  | CColor c -> Printf.sprintf "'%s'" c

and emit_expr env = function
  | EConst const -> emit_const const
  | EVar var -> emit_var env var
  | ECall (var, exprs, Loc (file, line)) -> (
      let name =
        match var with
        | VName (n, _) -> n
        | _ -> failwith "emit_expr: ECall: not a VName"
      in
      let compiled_exprs_list = List.map (emit_expr env) exprs in
      let compiled_exprs = String.concat "," compiled_exprs_list in
      match List.assoc_opt name env.ffi_map with
      (* TODO clean this up. add some kind of analysis pass on it, too. *)
      | Some template ->
          let result = ref template in
          List.iteri
            (fun i arg ->
              result :=
                Str.global_replace
                  (Str.regexp ("\\$" ^ string_of_int i))
                  arg !result)
            compiled_exprs_list;
          !result
      | None -> (
          (* TODO remove this when prototypes work *)
          match name with
          | "debug" ->
              Printf.sprintf "%s('[%s:%d]: ' + %s)" (emit_name name) file line
                compiled_exprs
          | "str" -> compiled_exprs
          | _ -> Printf.sprintf "%s(%s)" (emit_name name) compiled_exprs))
  | EBinary (bop, e1, e2, _) -> (
      let f = match bop with Div -> "Math.floor" | _ -> "" in
      let basic_binary o l r = Printf.sprintf "%s(%s %s %s)" f l o r in
      let c1 = emit_expr env e1 in
      let c2 = emit_expr env e2 in
      let op = emit_bop bop in
      match bop with
      | Eq -> Printf.sprintf "window.deepEqual(%s, %s)" c1 c2
      | Neq -> Printf.sprintf "! window.deepEqual(%s, %s)" c1 c2
      | _ -> basic_binary op c1 c2)
  | EUnary (uop, e, _) ->
      let c1 = emit_expr env e in
      let op = emit_uop uop in
      Printf.sprintf "(%s %s)" op c1
  | EList (exprs, _) ->
      let items = List.map (emit_expr env) exprs |> String.concat ", " in
      Printf.sprintf "[%s]" items
  | ERec (_, fields, _) ->
      let emit_field (n, e) = Printf.sprintf "%s:%s" n (emit_expr env e) in
      let f = List.map emit_field fields |> String.concat ", " in
      Printf.sprintf "{%s}" f
  | EEnum (name, member, _) -> Printf.sprintf "%s.%s" name member
  | ESafeBind _ -> failwith "unexpected safe bind"

let rec emit_stmt env = function
  | SVar (name, _, expr, _) ->
      Printf.sprintf "let %s = %s;" name (emit_expr env expr)
  | SMutate (var, expr, _) ->
      Printf.sprintf "%s = %s;" (emit_var env var) (emit_expr env expr)
  | SExpr (expr, _) -> emit_expr env expr
  | SIfElse (expr, block1, block2, _) -> (
      let b1 = emit_block env block1 in
      let b2 = Option.map (emit_block env) block2 in
      let b2_else =
        match b2 with Some b -> Printf.sprintf "else {\n%s\n}" b | None -> ""
      in
      match expr with
      | ESafeBind (bind_name, bind_expr, _) ->
          let be = emit_expr env bind_expr in
          let varbind = Printf.sprintf "let %s = %s;" bind_name be in
          Printf.sprintf "%s\nif (%s !== null) {\n%s\n} %s" varbind bind_name b1
            b2_else
      | _ ->
          let e = emit_expr env expr in
          Printf.sprintf "if (%s) {\n%s\n} %s" e b1 b2_else)
  | SFor (name, expr1, expr2, block, _) ->
      Printf.sprintf "for (let %s = %s; %s < %s; %s += 1) {\n%s\n}" name
        (emit_expr env expr1) name (emit_expr env expr2) name
        (emit_block env block)
  | SForIn (name, exp, block, _) ->
      Printf.sprintf "for (const %s of %s) {\n%s\n}" name (emit_expr env exp)
        (emit_block env block)
  | SRet (expr, _) -> Printf.sprintf "return %s;" (emit_expr env expr)
  | SMatch (expr, whens, _) ->
      let emit_when (e, b) =
        let ce = emit_expr env e in
        let be = emit_block env b in
        Printf.sprintf "case %s:\n%s\nbreak;\n" ce be
      in
      let e = emit_expr env expr in
      let ws = List.map emit_when whens |> String.concat "\n" in
      Printf.sprintf "switch (%s) {\n%s\n}" e ws
  | SBreak _ -> "break;"
  | SCond (whens, _) ->
      let emit_when (e, b) =
        let ce = emit_expr env e in
        let be = emit_block env b in
        Printf.sprintf "if (%s) {\n%s\n}\n" ce be
      in
      List.map emit_when whens |> String.concat " else "

and emit_block env (Block stmts) =
  String.concat "\n" (List.map (emit_stmt env) stmts)

let emit_toplevel env = function
  | TLStmt stmt -> emit_stmt env stmt
  | TLDef def -> (
      match def.ffi with
      (* don't emit ffi functions *)
      | Some _ -> ""
      | None ->
          let params = List.map fst def.params |> String.concat ", " in
          Printf.sprintf "function %s(%s) {\n%s\n}" def.name params
            (emit_block env def.body))
  | TLRec _ -> ""
  | TLLoad (name, src, _) ->
      Printf.sprintf "const %s = engine.preload('%s');" name src
  | TLConst (name, _, expr, _) ->
      Printf.sprintf "const %s = %s;" name (emit_expr env expr)
  | TLEnum (name, members, _) ->
      let member_as_field m = Printf.sprintf "%s:'%s'" m m in
      let members' = List.map member_as_field members in
      Printf.sprintf "const %s = {%s};" name (String.concat ", " members')

(* TODO: make the engine handle this *)
let add_empty_def name toplevels =
  let compare = function TLDef def -> def.name = name | _ -> false in
  match List.find_opt compare toplevels with
  | Some _ -> toplevels
  | None ->
      TLDef
        {
          name;
          params = [];
          body = Block [];
          loc = Loc ("", 0);
          ret = None;
          ffi = None;
        }
      :: toplevels

let ensure_engine_defs toplevels =
  add_empty_def "update" toplevels |> add_empty_def "draw"

let compile (Program toplevels) =
  let toplevels' = ensure_engine_defs toplevels in
  let env = { ffi_map = build_ffi_map toplevels' } in
  let body = String.concat "\n" (List.map (emit_toplevel env) toplevels') in
  Printf.sprintf
    "function game(engine) {\n%s\nreturn {update:update,draw:draw};\n}" body
