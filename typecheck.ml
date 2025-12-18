open Syntax
module StringSet = Set.Make (String)

type ty =
  | TyUnit
  | TyBool
  | TyInt
  | TyColor
  | TyStr
  | TyImage
  | TyFun of ty list * ty
  | TyList of ty
  | TyVar of ty option ref
  | TyRec of (string * ty) list
  | TyEnum of string * string list

let rec show_ty = function
  | TyUnit -> "unit"
  | TyBool -> "bool"
  | TyInt -> "int"
  | TyColor -> "color"
  | TyStr -> "str"
  | TyImage -> "image"
  | TyFun (tys, ty) ->
      let inner = List.map show_ty tys |> String.concat ", " in
      Printf.sprintf "((%s) -> %s)" inner (show_ty ty)
  | TyList ty -> "[" ^ show_ty ty ^ "]"
  | TyVar tyoptref -> (
      match !tyoptref with Some t -> show_ty t | None -> "?")
  | TyRec fields ->
      let field_ty (name, ty) = Printf.sprintf "%s: %s" name (show_ty ty) in
      let inner = List.map field_ty fields |> String.concat ", " in
      Printf.sprintf "rec(%s)" inner
  | TyEnum (name, _) -> Printf.sprintf "enum(%s)" name

exception TypeError of string * loc

let error loc msg = raise (TypeError (msg, loc))

type ventry = { ty : ty; const : bool }

type env = {
  tenv : (string * ty) list;
  venv : (string * ventry) list;
  ret : ty option;
  looping : bool;
}

let key_enum = TyEnum ("Key", [ "Left"; "Right"; "Up"; "Down"; "Z"; "X" ])

let base_tenv =
  [
    ("int", TyInt);
    ("bool", TyBool);
    ("color", TyColor);
    ("str", TyStr);
    ("image", TyImage);
    ("Key", key_enum);
  ]

let base_venv =
  [
    ("pset", { ty = TyFun ([ TyInt; TyInt; TyColor ], TyUnit); const = true });
    ("button", { ty = TyFun ([ key_enum ], TyBool); const = true });
    ("buttonp", { ty = TyFun ([ key_enum ], TyBool); const = true });
    ("clear", { ty = TyFun ([], TyUnit); const = true });
    ( "text",
      {
        ty = TyFun ([ TyStr; TyInt; TyInt; TyInt; TyColor ], TyUnit);
        const = true;
      } );
    ("debug", { ty = TyFun ([ TyStr ], TyUnit); const = true });
    ( "render",
      {
        ty =
          TyFun
            ( [ TyImage; TyInt; TyInt; TyInt; TyInt; TyInt; TyInt; TyInt; TyInt ],
              TyUnit );
        const = true;
      } );
    (* special forms that are here for name lookup but handled different in type checking *)
    (* forall a: a -> str *)
    ("str", { ty = TyFun ([ TyVar (ref None) ], TyStr); const = true });
    (* forall a: list[a] -> int *)
    ("len", { ty = TyFun ([ TyList (TyVar (ref None)) ], TyInt); const = true });
    (* forall a: a -> list[a] -> int *)
    ("push", { ty = TyFun ([], TyUnit); const = true });
    (* forall a: list[a] -> a *)
    ("pop", { ty = TyFun ([], TyUnit); const = true });
  ]

let lookup loc k e =
  match List.assoc_opt k e with
  | Some v -> v
  | None -> error loc ("symbol not found: " ^ k)

let mem = List.mem_assoc

let rec final_ty = function
  | TyVar r as t -> (
      match !r with
      | Some t' ->
          let final = final_ty t' in
          r := Some final;
          final
      | None -> t)
  | t -> t

let rec unify loc t1 t2 =
  let t1' = final_ty t1 in
  let t2' = final_ty t2 in
  match (t1', t2') with
  | _ when t1' = t2' -> ()
  | TyVar r1, _ -> r1 := Some t2'
  | _, TyVar r2 -> r2 := Some t1'
  | TyList l1, TyList l2 -> unify loc l1 l2
  | TyEnum (n1, _), TyEnum (n2, _) when n1 = n2 -> ()
  | _ ->
      error loc
        (Printf.sprintf
           "unifcation failed.\n\n\
            while trying to unify the following types:\n\
            type a = %s\n\
            type b = %s\n\n\
            unification failed at this step:\n\
            type c = %s\n\
            type d = %s"
           (show_ty t1) (show_ty t2) (show_ty t1') (show_ty t2'))

let has_duplicates fields =
  let n1 = List.length fields in
  let n2 = StringSet.(of_list fields |> to_list) |> List.length in
  not (n1 = n2)

let same_fields f1 f2 =
  if List.length f1 = List.length f2 then
    let s1 = StringSet.of_list f1 in
    let s2 = StringSet.of_list f2 in
    StringSet.(diff s1 s2 |> to_list) |> List.is_empty
  else false

let rec lookup_typing loc env = function
  | TName name -> lookup loc name env.tenv
  | TList typing -> TyList (lookup_typing loc env typing)

let rec check_var env var =
  let rec check = function
    | VName (name, loc) -> lookup loc name env.venv
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

and check_expr env expr =
  let rec check = function
    | EBool _ -> TyBool
    | EInt _ -> TyInt
    | EColor _ -> TyColor
    | EStr _ -> TyStr
    | EVar var ->
        let entry = check_var env var in
        entry.ty
    | ECall (var, exprs, loc) -> (
        let name =
          match var with
          | VName (n, _) -> n
          | _ -> error loc "not a valid function name"
        in
        let expr_tys = List.map check exprs in
        (* a very janky parametric polymorphism :) *)
        match name with
        | "len" ->
            if expr_tys = [ TyStr ] then TyInt
            else (
              List.iter2 (unify loc) [ TyList (TyVar (ref None)) ] expr_tys;
              TyInt)
        | "str" ->
            List.iter2 (unify loc) [ TyVar (ref None) ] expr_tys;
            TyStr
        | "push" ->
            let a = TyVar (ref None) in
            let f = [ a; TyList a ] in
            List.iter2 (unify loc) f expr_tys;
            TyInt
        | "pop" ->
            let a = TyVar (ref None) in
            let f = [ TyList a ] in
            List.iter2 (unify loc) f expr_tys;
            a
        | _ -> (
            let entry = lookup loc name env.venv in
            match entry.ty with
            | TyFun (param_tys, return_ty) ->
                List.iter2 (unify loc) param_tys expr_tys;
                return_ty
            | _ -> error loc ("not a function:: " ^ name)))
    | EBinary (bop, expr1, expr2, loc) -> (
        let t1 = check expr1 in
        let t2 = check expr2 in
        match bop with
        | Add ->
            (* a very janky function overloading :) *)
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
    | ERec (name, fields, loc) -> (
        let field_types (n, e) = (n, check e) in
        let fields' = List.map field_types fields |> List.sort compare in
        match lookup loc name env.tenv with
        | TyRec ts ->
            if same_fields (List.map fst ts) (List.map fst fields') then (
              (* fields' is sorted nd ts is sorted before adding to env *)
              List.iter2 (unify loc) (List.map snd ts) (List.map snd fields');
              TyRec ts)
            else error loc "missing record fields"
        | _ -> error loc ("not a record: " ^ name))
    | EEnum (name, member, loc) -> (
        match lookup loc name env.tenv with
        | TyEnum (_, members) ->
            (* return a member of enum so we can do exhaustiveness check on a match *)
            if List.mem member members then TyEnum (name, [ member ])
            else
              error loc
                (Printf.sprintf "%s is not a member of the enum %s" member name)
        | _ -> error loc (Printf.sprintf "%s is not an enum" name))
  in
  check expr

let check_var_decl ?(const = false) env name typing expr loc =
  if mem name env.venv then error loc ("duplicate symbol definition: " ^ name)
  else
    let ty = lookup_typing loc env typing in
    let expr_ty = check_expr env expr in
    unify loc ty expr_ty;
    let entry = { ty; const } in
    { env with venv = (name, entry) :: env.venv }

let rec check_stmt env stmt =
  match stmt with
  | SVar (name, typing, expr, loc) -> check_var_decl env name typing expr loc
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
      let expr_ty = check_expr env expr in
      unify loc TyBool expr_ty;
      let _ = check_block env block1 in
      let _ = Option.map (check_block env) block2 in
      env
  | SFor (name, expr1, expr2, block, loc) ->
      if mem name env.venv then
        error loc ("duplicate symbol definition: " ^ name)
      else (
        unify loc TyInt (check_expr env expr1);
        unify loc TyInt (check_expr env expr2);
        let entry = { ty = TyInt; const = false } in
        let env' =
          { env with venv = (name, entry) :: env.venv; looping = true }
        in
        let _ = check_block env' block in
        env)
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

and check_block env (Block stmts) = List.fold_left check_stmt env stmts

let check_toplevel env = function
  | TLStmt stmt -> check_stmt env stmt
  | TLDef def ->
      if mem def.name env.venv then
        error def.loc ("duplicate symbol definition: " ^ def.name)
      else
        let format_param (name, typing) =
          let entry =
            { ty = lookup_typing def.loc env typing; const = false }
          in
          (name, entry)
        in
        let params = List.map format_param def.params in
        let ret = Option.map (lookup_typing def.loc env) def.ret in
        let env' = { env with venv = params @ env.venv; ret } in
        let _ = check_block env' def.body in
        let tys = List.map (fun p -> (snd p).ty) params in
        let entry =
          { ty = TyFun (tys, Option.value ret ~default:TyUnit); const = true }
        in
        { env with venv = (def.name, entry) :: env.venv }
  | TLRec record ->
      if mem record.name env.venv then
        error record.loc ("duplicate symbol definition: " ^ record.name)
      else if has_duplicates (List.map fst record.fields) then
        error record.loc "duplicate field definitions"
      else
        let type_field (n, t) = (n, lookup_typing record.loc env t) in
        let fields' = List.map type_field record.fields |> List.sort compare in
        let t = TyRec fields' in
        { env with tenv = (record.name, t) :: env.tenv }
  | TLLoad (name, value, loc) ->
      if mem name env.venv then
        error loc ("duplicate symbol definition: " ^ name)
      else
        let filepath =
          Filename.concat (Filename.dirname (loc_get_file loc)) value
        in
        if Sys.file_exists filepath then
          let entry = { ty = TyImage; const = true } in
          { env with venv = (name, entry) :: env.venv }
        else error loc ("can't load asset: " ^ filepath)
  | TLConst (name, typing, expr, loc) ->
      check_var_decl ~const:true env name typing expr loc
  | TLEnum (name, members, loc) ->
      if mem name env.tenv then
        error loc ("duplicate symbol definition: " ^ name)
      else if has_duplicates members then error loc "duplicate enum members"
      else
        let t = TyEnum (name, members) in
        let tenv' = (name, t) :: env.tenv in
        { env with tenv = tenv' }

let check (Program toplevels) =
  let base_env =
    { tenv = base_tenv; venv = base_venv; ret = None; looping = false }
  in
  let _ = List.fold_left check_toplevel base_env toplevels in
  ()
