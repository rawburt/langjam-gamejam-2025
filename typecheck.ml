open Syntax

type ty =
  | TyUnit
  | TyBool
  | TyInt
  | TyColor
  | TyStr
  | TyFun of ty list * ty
  | TyList of ty
  | TyVar of ty option ref
[@@deriving show]

exception TypeError of string * loc

let error loc msg = raise (TypeError (msg, loc))

type env = {
  tenv : (string * ty) list;
  venv : (string * ty) list;
  ret : ty option;
}

let base_tenv =
  [ ("int", TyInt); ("bool", TyBool); ("color", TyColor); ("str", TyStr) ]

let base_venv =
  [
    ("pset", TyFun ([ TyInt; TyInt; TyColor ], TyUnit));
    ("button", TyFun ([ TyInt ], TyBool));
    ("buttonp", TyFun ([ TyInt ], TyBool));
    ("clear", TyFun ([], TyUnit));
    ("text", TyFun ([ TyStr; TyInt; TyInt; TyInt; TyColor ], TyUnit));
    ("debug", TyFun ([ TyStr ], TyUnit));
    (* special forms that are here for name lookup but handled different in type checking *)
    (* forall a: a -> str *)
    ("str", TyFun ([ TyVar (ref None) ], TyStr));
    (* forall a: list[a] -> int *)
    ("len", TyFun ([ TyList (TyVar (ref None)) ], TyInt));
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
  | _ ->
      error loc
        (Printf.sprintf "unifcation failed.\nt1' = %s\nt2' = %s\n" (show_ty t1')
           (show_ty t2'))

let rec lookup_typing loc env = function
  | TName name -> lookup loc name env.tenv
  | TList typing -> TyList (lookup_typing loc env typing)

let rec check_var env var =
  let rec check = function
    | VName (name, loc) -> lookup loc name env.venv
    | VSub (v, expr, loc) -> (
        match check v with
        | TyList t -> (
            match check_expr env expr with
            | TyInt -> t
            | _ -> error loc "subscript expression must be an integer")
        | _ -> error loc "not a list")
  in
  check var

and check_expr env expr =
  let rec check = function
    | EBool _ -> TyBool
    | EInt _ -> TyInt
    | EColor _ -> TyColor
    | EStr _ -> TyStr
    | EVar var -> check_var env var
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
        | _ -> (
            match lookup loc name env.venv with
            | TyFun (param_tys, return_ty) ->
                List.iter2 (unify loc) param_tys expr_tys;
                return_ty
            | _ -> error loc "not a function"))
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
        | Sub ->
            unify loc TyInt t1;
            unify loc TyInt t2;
            TyInt
        | Mul ->
            unify loc TyInt t1;
            unify loc TyInt t2;
            TyInt
        | Lt ->
            unify loc TyInt t1;
            unify loc TyInt t2;
            TyBool
        | Gt ->
            unify loc TyInt t1;
            unify loc TyInt t2;
            TyBool
        | Eq ->
            unify loc t1 t2;
            TyBool
        | Or ->
            unify loc TyBool t1;
            unify loc TyBool t2;
            TyBool
        | And ->
            unify loc TyBool t1;
            unify loc TyBool t2;
            TyBool)
    | EList (exprs, loc) -> (
        let tys = List.map check exprs in
        match tys with
        | [] -> TyList (TyVar (ref None))
        | [ t ] -> TyList t
        | t :: ts ->
            List.iter (unify loc t) ts;
            TyList t)
  in
  check expr

let rec check_stmt env stmt =
  match stmt with
  | SVar (name, typing, expr, loc) ->
      if mem name env.venv then
        error loc ("duplicate symbol definition: " ^ name)
      else
        let ty = lookup_typing loc env typing in
        let expr_ty = check_expr env expr in
        unify loc ty expr_ty;
        { env with venv = (name, ty) :: env.venv }
  | SMutate (var, expr, loc) ->
      let ty = check_var env var in
      let expr_ty = check_expr env expr in
      unify loc ty expr_ty;
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
        let env' = { env with venv = (name, TyInt) :: env.venv } in
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

and check_block env (Block stmts) = List.fold_left check_stmt env stmts

let check_toplevel env = function
  | TLStmt stmt -> check_stmt env stmt
  | TLDef def ->
      if mem def.name env.venv then
        error def.loc ("duplicate symbol definition: " ^ def.name)
      else
        let params =
          List.map
            (fun (name, typing) -> (name, lookup_typing def.loc env typing))
            def.params
        in
        let ret = Option.map (lookup_typing def.loc env) def.ret in
        let env' = { env with venv = params @ env.venv; ret } in
        let _ = check_block env' def.body in
        let tys = List.map snd params in
        {
          env with
          venv =
            (def.name, TyFun (tys, Option.value ret ~default:TyUnit))
            :: env.venv;
        }

let check (Program toplevels) =
  let base_env = { tenv = base_tenv; venv = base_venv; ret = None } in
  let _ = List.fold_left check_toplevel base_env toplevels in
  ()
