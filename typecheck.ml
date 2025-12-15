open Syntax

type ty =
  | TyUnit
  | TyBool
  | TyInt
  | TyColor
  | TyFun of ty list * ty
  | TyList of ty
  | TyVar of ty option ref
[@@deriving show]

exception TypeError of string

let error msg = raise (TypeError msg)

type env = { tenv : (string * ty) list; venv : (string * ty) list }

let base_tenv = [ ("int", TyInt); ("bool", TyBool); ("color", TyColor) ]

let base_venv =
  [
    ("pset", TyFun ([ TyInt; TyInt; TyColor ], TyUnit));
    ("button", TyFun ([ TyInt ], TyBool));
  ]

let lookup k e =
  match List.assoc_opt k e with
  | Some v -> v
  | None -> error ("symbol not found: " ^ k)

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

let rec unify t1 t2 =
  let t1' = final_ty t1 in
  let t2' = final_ty t2 in
  match (t1', t2') with
  | _ when t1' = t2' -> ()
  | TyVar r1, _ -> r1 := Some t2'
  | _, TyVar r2 -> r2 := Some t1'
  | TyList l1, TyList l2 -> unify l1 l2
  | _ ->
      error
        (Printf.sprintf "unifcation failed.\nt1' = %s\nt2' = %s\n" (show_ty t1')
           (show_ty t2'))

let rec lookup_typing env = function
  | TName name -> lookup name env.tenv
  | TList typing -> TyList (lookup_typing env typing)

let check_expr env expr =
  let rec check = function
    | EBool _ -> TyBool
    | EInt _ -> TyInt
    | EColor _ -> TyColor
    | EVar (VName name) -> lookup name env.venv
    | ECall (VName name, exprs) -> (
        let expr_tys = List.map check exprs in
        match lookup name env.venv with
        | TyFun (param_tys, return_ty) ->
            List.iter2 unify param_tys expr_tys;
            return_ty
        | _ -> error "not a function")
    | EBinary (bop, expr1, expr2) -> (
        let t1 = check expr1 in
        let t2 = check expr2 in
        match bop with
        | Add ->
            unify TyInt t1;
            unify TyInt t2;
            TyInt)
    | EList exprs -> (
        let tys = List.map check exprs in
        match tys with
        | [] -> TyList (TyVar (ref None))
        | [ t ] -> TyList t
        | t :: ts ->
            List.iter (unify t) ts;
            TyList t)
  in
  check expr

let rec check_stmt env stmt =
  match stmt with
  | SVar (name, typing, expr) ->
      if mem name env.venv then error ("duplicate symbol definition: " ^ name)
      else
        let ty = lookup_typing env typing in
        let expr_ty = check_expr env expr in
        unify ty expr_ty;
        { env with venv = (name, ty) :: env.venv }
  | SMutate (name, expr) ->
      if mem name env.venv then (
        let ty = lookup name env.venv in
        let expr_ty = check_expr env expr in
        unify ty expr_ty;
        env)
      else error ("symbol not declared: " ^ name)
  | SExpr expr ->
      let _ = check_expr env expr in
      env
  | SIfElse (expr, block1, block2) ->
      let expr_ty = check_expr env expr in
      unify TyBool expr_ty;
      let _ = check_block env block1 in
      let _ = check_block env block2 in
      env
  | SFor (name, expr1, expr2, block) ->
      if mem name env.venv then error ("duplicate symbol definition: " ^ name)
      else (
        unify TyInt (check_expr env expr1);
        unify TyInt (check_expr env expr2);
        let env' = { env with venv = (name, TyInt) :: env.venv } in
        let _ = check_block env' block in
        env)

and check_block env (Block stmts) = List.fold_left check_stmt env stmts

let check_toplevel env = function
  | TLStmt stmt -> check_stmt env stmt
  | TLDef def ->
      if mem def.name env.venv then
        error ("duplicate symbol definition: " ^ def.name)
      else
        let params =
          List.map
            (fun (name, typing) -> (name, lookup_typing env typing))
            def.params
        in
        let env' = { env with venv = params @ env.venv } in
        let _ = check_block env' def.body in
        let tys = List.map snd params in
        { env with venv = (def.name, TyFun (tys, TyUnit)) :: env.venv }

let check (Program toplevels) =
  let base_env = { tenv = base_tenv; venv = base_venv } in
  let _ = List.fold_left check_toplevel base_env toplevels in
  ()
