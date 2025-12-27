open Syntax
module StringMap = Map.Make (String)

let next_id =
  (* start at 50 so builtins don't collide *)
  let counter = ref 50 in
  fun () ->
    let id = !counter in
    incr counter;
    id

let all_unique lst =
  List.length lst = List.length (List.sort_uniq Stdlib.compare lst)

(* TODO vars and methods separate *)
type env = { vars : int StringMap.t list; types : int StringMap.t }

let push_scope env = { env with vars = StringMap.empty :: env.vars }

(* does name exist in current scope *)
let var_mem_scope env name =
  match env.vars with
  | [] -> failwith "var_mem_scope: no scope exists"
  | scope :: _ -> StringMap.mem name scope

(* does name exist in any scope *)
let var_mem env name =
  List.exists (fun scope -> StringMap.mem name scope) env.vars

(* add name to current scope *)
let var_add env name =
  match env.vars with
  | [] -> failwith "var_add: no scope exists"
  | scope :: rest ->
      let next_id = next_id () in
      let new_scope = StringMap.add name next_id scope in
      ({ env with vars = new_scope :: rest }, next_id)

let var_find env name =
  let rec loop = function
    | [] -> None
    | scope :: rest -> (
        match StringMap.find_opt name scope with
        | Some id -> Some id
        | None -> loop rest)
  in
  loop env.vars

let type_add env name =
  let next_id = next_id () in
  let new_types = StringMap.add name next_id env.types in
  ({ env with types = new_types }, next_id)

let error loc msg = raise (Errors.AnalysisError (msg, loc))

let base_types =
  [
    ("int", 1); ("bool", 2); ("color", 3); ("str", 4); ("image", 5); ("Key", 6);
  ]
  |> StringMap.of_list

let base_vars = [ [ ("debug", 7) ] |> StringMap.of_list ]
let base_env = { vars = base_vars; types = base_types }

let rec resolve_typing env = function
  | TName ((name, _), loc) -> (
      match StringMap.find_opt name env.types with
      | Some id -> TName ((name, id), loc)
      | None -> error loc (Printf.sprintf "unknown type: %s" name))
  | TList (typing, loc) ->
      let resolved_typing = resolve_typing env typing in
      TList (resolved_typing, loc)
  | TOpt (typing, loc) ->
      let resolved_typing = resolve_typing env typing in
      TOpt (resolved_typing, loc)
  | TParam ((name, _), loc) -> (
      match StringMap.find_opt name env.types with
      | Some id -> TParam ((name, id), loc)
      | None -> error loc (Printf.sprintf "unknown type parameter: %s" name))
  | TUnion (typings, loc) ->
      let resolved_typings = List.map (resolve_typing env) typings in
      TUnion (resolved_typings, loc)

let rec resolve_var env = function
  | VName ((name, _), loc) -> (
      match var_find env name with
      | Some id -> VName ((name, id), loc)
      | None -> error loc (Printf.sprintf "undefined variable: %s" name))
  | VSub (var, expr, loc) ->
      let resolved_var = resolve_var env var in
      let resolved_expr = resolve_expr env expr in
      VSub (resolved_var, resolved_expr, loc)
  | VField (var, field, loc) ->
      let resolved_var = resolve_var env var in
      VField (resolved_var, field, loc)

and resolve_expr env = function
  | EConst const -> EConst const
  | EVar var -> EVar (resolve_var env var)
  | ECall (var, args, loc) ->
      let resolved_var = resolve_var env var in
      let resolved_args = List.map (resolve_expr env) args in
      ECall (resolved_var, resolved_args, loc)
  | EBinary (bop, expr1, expr2, loc) ->
      let resolved_expr1 = resolve_expr env expr1 in
      let resolved_expr2 = resolve_expr env expr2 in
      EBinary (bop, resolved_expr1, resolved_expr2, loc)
  | EUnary (uop, expr, loc) ->
      let resolved_expr = resolve_expr env expr in
      EUnary (uop, resolved_expr, loc)
  | EList (exprs, loc) ->
      let resolved_exprs = List.map (resolve_expr env) exprs in
      EList (resolved_exprs, loc)
  | ERec ((name, _), fields, loc) -> (
      let resolve_field (field_name, expr) =
        let resolved_expr = resolve_expr env expr in
        (field_name, resolved_expr)
      in
      let resolved_fields = List.map resolve_field fields in
      match StringMap.find_opt name env.types with
      | Some id -> ERec ((name, id), resolved_fields, loc)
      | None -> error loc (Printf.sprintf "unknown record type: %s" name))
  | EEnum ((name, _), variant, loc) -> (
      match StringMap.find_opt name env.types with
      | Some id -> EEnum ((name, id), variant, loc)
      | None -> error loc (Printf.sprintf "unknown enum type: %s" name))
  | ESafeBind (_, _, loc) ->
      error loc "safe bind can only appear in if expressions"

and resolve_stmt env = function
  | SVar ((name, _), typing, expr, loc) ->
      (* look for variable in current scope otherwise declare it. supports shadowing.  *)
      if var_mem_scope env name then
        error loc (Printf.sprintf "variable already defined in scope: %s" name)
      else
        let resolved_typing = resolve_typing env typing in
        let resolved_expr = resolve_expr env expr in
        let env', next_id = var_add env name in
        (env', SVar ((name, next_id), resolved_typing, resolved_expr, loc))
  | SMutate (var, expr, loc) ->
      let resolved_var = resolve_var env var in
      let resolved_expr = resolve_expr env expr in
      (env, SMutate (resolved_var, resolved_expr, loc))
  | SExpr (expr, loc) ->
      let resolved_expr = resolve_expr env expr in
      (env, SExpr (resolved_expr, loc))
  | SIfElse (expr, block, block_opt, loc) ->
      let env_with_bind, resolved_expr =
        match expr with
        | ESafeBind ((name, _), bind_expr, loc) ->
            if var_mem_scope env name then
              error loc
                (Printf.sprintf "variable already defined in scope: %s" name)
            else
              let resolved_bind_expr = resolve_expr env bind_expr in
              let env', next_id = var_add env name in
              (env', ESafeBind ((name, next_id), resolved_bind_expr, loc))
        | _ ->
            let resolved_expr = resolve_expr env expr in
            (env, resolved_expr)
      in
      (* var is only available within the `then` block *)
      let _, resolved_block = resolve_block env_with_bind block in
      let resolved_block_opt =
        Option.map (fun b -> snd (resolve_block env b)) block_opt
      in
      (env, SIfElse (resolved_expr, resolved_block, resolved_block_opt, loc))
  | SFor ((name, _), start_expr, end_expr, block, loc) ->
      let resolved_start_expr = resolve_expr env start_expr in
      let resolved_end_expr = resolve_expr env end_expr in
      if var_mem_scope env name then
        error loc (Printf.sprintf "variable already defined in scope: %s" name)
      else
        (* push scope so var is not accessible outside the loop *)
        let env', next_id = var_add (push_scope env) name in
        let _, resolved_block = resolve_block env' block in
        ( env,
          SFor
            ( (name, next_id),
              resolved_start_expr,
              resolved_end_expr,
              resolved_block,
              loc ) )
  | SForIn ((name, _), expr, block, loc) ->
      let resolved_expr = resolve_expr env expr in
      if var_mem_scope env name then
        error loc (Printf.sprintf "variable already defined in scope: %s" name)
      else
        (* push scope so var is not accessible outside the loop *)
        let env', next_id = var_add (push_scope env) name in
        let _, resolved_block = resolve_block env' block in
        (env, SForIn ((name, next_id), resolved_expr, resolved_block, loc))
  | SRet (expr, loc) ->
      let resolved_expr = resolve_expr env expr in
      (env, SRet (resolved_expr, loc))
  | SMatch (expr, whens, loc) ->
      let resolved_expr = resolve_expr env expr in
      let resolve_when env (when_expr, block) =
        let resolved_when_expr = resolve_expr env when_expr in
        let _, resolved_block = resolve_block env block in
        (resolved_when_expr, resolved_block)
      in
      let resolved_whens = List.map (resolve_when env) whens in
      (env, SMatch (resolved_expr, resolved_whens, loc))
  | SBreak loc -> (env, SBreak loc)
  | SCond (whens, loc) ->
      let resolve_when env (cond_expr, block) =
        let resolved_cond_expr = resolve_expr env cond_expr in
        let _, resolved_block = resolve_block env block in
        (resolved_cond_expr, resolved_block)
      in
      let resolved_whens = List.map (resolve_when env) whens in
      (env, SCond (resolved_whens, loc))

and resolve_block env (Block stmts) =
  let _, resolved_stmts =
    List.fold_left_map
      (fun env stmt -> resolve_stmt env stmt)
      (* push scope to support shadowing *)
      (push_scope env)
      stmts
  in
  (env, Block resolved_stmts)

let resolve_def env (def : def) =
  (* can't redefine defs *)
  let name, _ = def.name in
  if var_mem env name then
    error def.loc (Printf.sprintf "definition already exists: %s" name)
  else
    let env_with_def, def_id = var_add env name in
    let resolve_type_param env (type_param_name, _) =
      (* no shadowing for types. this also works as a uniqueness check. *)
      if StringMap.mem type_param_name env.types then
        error def.loc (Printf.sprintf "type already exists: %s" type_param_name)
      else
        let env', type_param_id = type_add env type_param_name in
        (env', (type_param_name, type_param_id))
    in
    let env_with_params, resolved_type_params =
      List.fold_left_map resolve_type_param env_with_def def.type_params
    in
    (* resolve return type after type parameters so return type can be a type param *)
    let resolved_ret = Option.map (resolve_typing env_with_params) def.ret in
    (* params are a new scope so they can shadow variables in outer scopes *)
    let scoped_env = push_scope env_with_params in
    let resolve_param env ((param_name, _), typing) =
      (* TODO: add params and resolve types *)
      let resolved_typing = resolve_typing env typing in
      (* TODO param name unique -- check for exist first *)
      let env', id = var_add env param_name in
      (env', ((param_name, id), resolved_typing))
    in
    let env_for_body, resolved_params =
      List.fold_left_map resolve_param scoped_env def.params
    in
    let _, resolved_body = resolve_block env_for_body def.body in
    let resolved_def =
      {
        name = (name, def_id);
        type_params = resolved_type_params;
        params = resolved_params;
        ret = resolved_ret;
        body = resolved_body;
        ffi = def.ffi;
        loc = def.loc;
      }
    in
    (* only def should be added to the global type environment *)
    (env_with_def, resolved_def)

let resolve_toplevel env = function
  | TLStmt stmt ->
      let env', resolved_stmt = resolve_stmt env stmt in
      (env', TLStmt resolved_stmt)
  | TLDef def ->
      Common.trace ("resolving: def " ^ fst def.name);
      let env', resolved_def = resolve_def env def in
      (env', TLDef resolved_def)
  | TLRec record ->
      Common.trace ("resolving: rec " ^ fst record.name);
      let name, _ = record.name in
      if StringMap.mem name env.types then
        error record.loc (Printf.sprintf "type already exists: %s" name)
      else
        let env', record_id = type_add env name in
        let resolve_field env (field_name, typing) =
          let resolved_typing = resolve_typing env typing in
          (field_name, resolved_typing)
        in
        let resolved_fields = List.map (resolve_field env') record.fields in
        if not (all_unique (List.map fst record.fields)) then
          error record.loc
            (Printf.sprintf "record fields must be unique in record: %s" name);
        let resolved_record =
          {
            name = (name, record_id);
            fields = resolved_fields;
            loc = record.loc;
          }
        in
        (env', TLRec resolved_record)
  | TLLoad ((name, _), path, loc) ->
      if var_mem env name then
        error loc (Printf.sprintf "variable already defined in scope: %s" name)
      else
        let env', def_id = var_add env name in
        (* TODO: check file exists *)
        (env', TLLoad ((name, def_id), path, loc))
  | TLConst ((name, _), typing, expr, loc) ->
      Common.trace ("resolving: const " ^ name);
      if var_mem env name then
        error loc (Printf.sprintf "variable already defined in scope: %s" name)
      else
        let resolved_typing = resolve_typing env typing in
        let resolved_expr = resolve_expr env expr in
        let env', const_id = var_add env name in
        (env', TLConst ((name, const_id), resolved_typing, resolved_expr, loc))
  | TLEnum ((name, _), members, loc) ->
      Common.trace ("resolving: enum " ^ name);
      if StringMap.mem name env.types then
        error loc (Printf.sprintf "type already exists: %s" name)
      else
        let env', enum_id = type_add env name in
        let resolved_enum = TLEnum ((name, enum_id), members, loc) in
        if not (all_unique members) then
          error loc
            (Printf.sprintf "enum members must be unique in enum: %s" name);
        (env', resolved_enum)

let run (Program toplevels) =
  let _, resolved_toplevels =
    List.fold_left_map resolve_toplevel base_env toplevels
  in
  Program resolved_toplevels
