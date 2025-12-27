open Syntax
open Ty
module StringSet = Set.Make (String)

exception TypeError of string * loc

let error loc msg = raise (TypeError (msg, loc))

module IntMap = Map.Make (Int)

type entry = { ty : ty; const : bool }
type env = { types : entry IntMap.t; ret : ty option; looping : bool }

let key_enum = TyEnum ("Key", [ "Left"; "Right"; "Up"; "Down"; "X" ])

let base_types =
  [
    (* int *)
    (1, TyInt);
    (* bool *)
    (2, TyBool);
    (* color *)
    (3, TyColor);
    (* str *)
    (4, TyStr);
    (* image *)
    (5, TyImage);
    (* Key *)
    (6, key_enum);
    (* debug *)
    (7, TyFun ([ TyStr ], TyUnit));
  ]
  |> List.map (fun (id, ty) -> (id, { ty; const = true }))
  |> IntMap.of_list

let instantiate =
  let rec inst_ty ty =
    match ty with
    | TyVarNamed _ -> TyVar (ref None)
    | TyFun (params, ret) -> TyFun (List.map inst_ty params, inst_ty ret)
    | TyList t -> TyList (inst_ty t)
    | TyOpt t -> TyOpt (inst_ty t)
    | TyUnion tys -> TyUnion (List.map inst_ty tys)
    | TyRec fields -> TyRec (List.map (fun (n, t) -> (n, inst_ty t)) fields)
    | t -> t
  in
  inst_ty

let rec final_ty = function
  | TyVar r as t -> (
      match !r with
      | Some t' ->
          let final = final_ty t' in
          r := Some final;
          final
      | None -> t)
  | t -> t

let unify loc ty1 ty2 =
  let rec uni t1 t2 =
    let t1' = final_ty t1 in
    let t2' = final_ty t2 in
    match (t1', t2') with
    | _ when t1' = t2' -> true
    | TyVar r1, _ ->
        r1 := Some t2';
        true
    | _, TyVar r2 ->
        r2 := Some t1';
        true
    | TyUnion tys, t2 -> List.exists (fun t1 -> uni t1 t2) tys
    | t1, TyUnion tys -> List.exists (fun t2 -> uni t1 t2) tys
    | TyList l1, TyList l2 -> uni l1 l2
    | TyOpt _, TyNull -> true
    | TyOpt o1, o2 when o1 = o2 -> true
    | TyEnum (n1, _), TyEnum (n2, _) when n1 = n2 -> true
    | _ -> false
  in
  if uni ty1 ty2 then ()
  else
    error loc
      (Printf.sprintf
         "type mismatch\n\nunification failed:\ntype 1 = %s\ntype 2 = %s"
         (show_ty ty1) (show_ty ty2))

let same_fields f1 f2 =
  if List.length f1 = List.length f2 then
    let s1 = StringSet.of_list f1 in
    let s2 = StringSet.of_list f2 in
    StringSet.(diff s1 s2 |> to_list) |> List.is_empty
  else false

let rec lookup_typing env = function
  | TName ((_name, id), _loc) -> (IntMap.find id env.types).ty
  | TList (typing, _loc) -> TyList (lookup_typing env typing)
  | TOpt (typing, _loc) -> TyOpt (lookup_typing env typing)
  | TParam ((name, _id), _loc) -> TyVarNamed name
  | TUnion (ts, _loc) ->
      let tys = List.map (lookup_typing env) ts in
      TyUnion tys

let rec check_var env var =
  let rec check = function
    | VName ((_name, id), _loc) -> IntMap.find id env.types
    | VSub (v, expr, loc) -> (
        match check_expr env expr with
        | TyInt -> (
            let sub_entry = check v in
            match sub_entry.ty with
            | TyList t -> { ty = t; const = sub_entry.const }
            | TyStr -> { ty = TyStr; const = sub_entry.const }
            | _ -> error loc "not a list or a str")
        | _ -> error loc "subscript expression must be an integer")
    | VField (v, name, loc) -> (
        let field_entry = check v in
        match field_entry.ty with
        | TyRec fields -> (
            match List.assoc_opt name fields with
            | Some t -> { ty = t; const = field_entry.const }
            | None -> error loc ("record does not have field name: " ^ name))
        | _ -> error loc "not a record")
  in
  check var

and check_const = function
  | CNull -> TyNull
  | CBool _ -> TyBool
  | CInt _ -> TyInt
  | CColor _ -> TyColor
  | CStr _ -> TyStr

and check_expr env expr =
  let rec check = function
    | EConst const -> check_const const
    | EVar var ->
        let entry = check_var env var in
        entry.ty
    | ECall (var, exprs, loc) -> (
        let name, id =
          match var with
          | VName (n, _) -> n
          | _ -> error loc "not a valid function name"
        in
        let expr_tys = List.map check exprs in
        let entry = IntMap.find id env.types in
        let instantiated_ty = instantiate entry.ty in
        match instantiated_ty with
        | TyFun (param_tys, return_ty) ->
            List.iter2 (unify loc) param_tys expr_tys;
            return_ty
        | _ -> error loc ("not a function:: " ^ name))
    | EBinary (bop, expr1, expr2, loc) -> (
        let t1 = check expr1 in
        let t2 = check expr2 in
        match bop with
        | Add ->
            (* TODO: add def overloads? *)
            if t1 = TyStr then (
              unify loc TyStr t1;
              unify loc TyStr t2;
              TyStr)
            else (
              unify loc TyInt t1;
              unify loc TyInt t2;
              TyInt)
        | Sub | Mul | Div | Mod ->
            unify loc TyInt t1;
            unify loc TyInt t2;
            TyInt
        | Lt | Gt | Lte | Gte ->
            unify loc TyInt t1;
            unify loc TyInt t2;
            TyBool
        | Eq ->
            unify loc t1 t2;
            TyBool
        | Neq ->
            unify loc t1 t2;
            TyBool
        | Or | And ->
            unify loc TyBool t1;
            unify loc TyBool t2;
            TyBool)
    | EUnary (uop, expr, loc) -> (
        let t = check expr in
        match uop with
        | Minus ->
            unify loc TyInt t;
            TyInt
        | Negate ->
            unify loc TyBool t;
            TyBool)
    | EList (exprs, loc) -> (
        let tys = List.map check exprs in
        match tys with
        | [] -> TyList (TyVar (ref None))
        | [ t ] -> TyList t
        | t :: ts ->
            List.iter (unify loc t) ts;
            TyList t)
    | ERec ((name, id), fields, loc) -> (
        let field_types (n, e) = (n, check e) in
        let fields' = List.map field_types fields |> List.sort compare in
        match (IntMap.find id env.types).ty with
        | TyRec ts ->
            if same_fields (List.map fst ts) (List.map fst fields') then (
              (* fields' is sorted nd ts is sorted before adding to env *)
              List.iter2 (unify loc) (List.map snd ts) (List.map snd fields');
              TyRec ts)
            else error loc "missing record fields"
        | _ -> error loc ("not a record: " ^ name))
    | EEnum ((name, id), member, loc) -> (
        match (IntMap.find id env.types).ty with
        | TyEnum (_, members) ->
            (* return a member of enum so we can do exhaustiveness check on a match *)
            if List.mem member members then TyEnum (name, [ member ])
            else
              error loc
                (Printf.sprintf "%s is not a member of the enum %s" member name)
        | _ -> error loc (Printf.sprintf "%s is not an enum" name))
    | ESafeBind (_, _, loc) -> error loc "unexpected safe bind"
  in
  check expr

let check_var_decl ?(const = false) env id typing expr loc =
  let ty = lookup_typing env typing in
  let expr_ty = check_expr env expr in
  unify loc ty expr_ty;
  let entry = { ty; const } in
  { env with types = IntMap.add id entry env.types }

let rec check_stmt env stmt =
  match stmt with
  | SVar ((_name, id), typing, expr, loc) ->
      check_var_decl env id typing expr loc
  | SMutate (var, expr, loc) ->
      let entry = check_var env var in
      if entry.const then error loc "can't mutate const"
      else
        let expr_ty = check_expr env expr in
        unify loc entry.ty expr_ty;
        env
  | SExpr (expr, _) ->
      let _ = check_expr env expr in
      env
  | SIfElse (expr, block1, block2, loc) ->
      let env' =
        match expr with
        | ESafeBind ((_name, id), e, eloc) -> (
            match check_expr env e with
            | TyOpt ty ->
                {
                  env with
                  types = IntMap.add id { ty; const = false } env.types;
                }
            | _ -> error eloc "safe bind requires nullable expression")
        | _ ->
            let expr_ty = check_expr env expr in
            unify loc TyBool expr_ty;
            env
      in
      let _ = check_block env' block1 in
      let _ = Option.map (check_block env) block2 in
      env
  | SFor ((_name, id), expr1, expr2, block, loc) ->
      unify loc TyInt (check_expr env expr1);
      unify loc TyInt (check_expr env expr2);
      let entry = { ty = TyInt; const = false } in
      let env' =
        { env with types = IntMap.add id entry env.types; looping = true }
      in
      let _ = check_block env' block in
      env
  | SForIn ((_name, id), expr, block, loc) ->
      let expr_ty = check_expr env expr in
      let t =
        match expr_ty with
        | TyList ty -> ty
        | TyStr -> TyStr
        | _ -> error loc "list or str required"
      in
      let entry = { ty = t; const = false } in
      let env' =
        { env with types = IntMap.add id entry env.types; looping = true }
      in
      let _ = check_block env' block in
      env
  | SRet (expr, loc) -> (
      let ty = check_expr env expr in
      match env.ret with
      | Some t ->
          unify loc t ty;
          env
      | None ->
          error loc "ret not allowed when function has no defined return value")
  | SMatch (expr, when_exprs, loc) -> (
      match check_expr env expr with
      | TyEnum (name, members) as t ->
          let check_when (expr, block) =
            let expr_ty = check_expr env expr in
            unify loc t expr_ty;
            let _ = check_block env block in
            match expr_ty with
            | TyEnum (_, [ member ]) -> member
            | _ -> failwith "oops something is very wrong"
          in
          let when_members = List.map check_when when_exprs in
          if members = when_members then env
          else error loc ("match is not exhaustive for enum " ^ name)
      | _ -> error loc "can only match on enums")
  | SBreak loc ->
      if env.looping then env else error loc "break must be inside a loop"
  | SCond (whens, loc) ->
      let check_when (expr, block) =
        unify loc TyBool (check_expr env expr);
        let _ = check_block env block in
        ()
      in
      List.iter check_when whens;
      env

and check_block env (Block stmts) = List.fold_left check_stmt env stmts

let check_toplevel env = function
  | TLStmt stmt -> check_stmt env stmt
  | TLDef def ->
      if Option.is_some def.ffi && def.body <> Block [] then
        error def.loc "ffi with body is not allowed"
      else
        (* add type params to type env *)
        let load_type_param env (type_param_name, param_id) =
          let t = TyVarNamed type_param_name in
          {
            env with
            types = IntMap.add param_id { ty = t; const = true } env.types;
          }
        in
        let original_types = env.types in
        let env_with_type_params =
          List.fold_left load_type_param env def.type_params
        in
        let format_param ((_name, id), typing) =
          let entry =
            { ty = lookup_typing env_with_type_params typing; const = false }
          in
          (id, entry)
        in
        let params = List.map format_param def.params in
        let params_map = params |> IntMap.of_list in
        let ret = Option.map (lookup_typing env_with_type_params) def.ret in
        let env_types =
          IntMap.union
            (fun _ _ v -> Some v)
            params_map env_with_type_params.types
        in
        let env_with_params =
          { env_with_type_params with types = env_types; ret }
        in
        let _ = check_block env_with_params def.body in
        let tys = List.map (fun p -> (snd p).ty) params in
        let entry =
          { ty = TyFun (tys, Option.value ret ~default:TyUnit); const = true }
        in
        { env with types = IntMap.add (snd def.name) entry original_types }
  | TLRec record ->
      let type_field (n, t) = (n, lookup_typing env t) in
      let fields' = List.map type_field record.fields |> List.sort compare in
      let t = TyRec fields' in
      {
        env with
        types = IntMap.add (snd record.name) { ty = t; const = true } env.types;
      }
  | TLLoad ((_name, id), value, loc) ->
      let filepath =
        Filename.concat (Filename.dirname (loc_get_file loc)) value
      in
      (* TODO do this in name resolution pass *)
      if Sys.file_exists filepath then
        {
          env with
          types = IntMap.add id { ty = TyImage; const = true } env.types;
        }
      else error loc ("can't load asset: " ^ filepath)
  | TLConst ((_name, id), typing, expr, loc) ->
      check_var_decl ~const:true env id typing expr loc
  | TLEnum ((name, id), members, _) ->
      let t = TyEnum (name, members) in
      { env with types = IntMap.add id { ty = t; const = true } env.types }

let check (Program toplevels) =
  let base_env = { types = base_types; ret = None; looping = false } in
  let _ = List.fold_left check_toplevel base_env toplevels in
  ()
