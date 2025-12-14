open Syntax

type ty = TUnit | TBool | TInt | TColor | TFun of ty list * ty

exception TypeError of string

type env = { tenv : (string * ty) list; venv : (string * ty) list }

let base_tenv = [ ("int", TInt); ("bool", TBool); ("color", TColor) ]

let base_venv =
  [
    ("pset", TFun ([ TInt; TInt; TColor ], TUnit));
    ("button", TFun ([ TInt ], TBool));
  ]

let lookup k e =
  match List.assoc_opt k e with
  | Some v -> v
  | None -> raise (TypeError ("symbol not found: " ^ k))

let mem = List.mem_assoc

let expect_tys ts1 ts2 =
  if ts1 = ts2 then () else raise (TypeError "type mismatch")

let expect_ty t1 t2 = if t1 = t2 then () else raise (TypeError "type mismatch")

let expect_ty3 t1 t2 t3 =
  if t1 = t2 && t1 = t3 then () else raise (TypeError "type mismatch")

let check_typing env = function TName name -> lookup name env.tenv

let check_expr env expr =
  let rec check = function
    | EBool _ -> TBool
    | EInt _ -> TInt
    | EColor _ -> TColor
    | EVar (VName name) -> lookup name env.venv
    | ECall (VName name, exprs) -> (
        let expr_tys = List.map check exprs in
        match lookup name env.venv with
        | TFun (param_tys, return_ty) ->
            expect_tys param_tys expr_tys;
            return_ty
        | _ -> raise (TypeError "not a function"))
    | EBinary (bop, expr1, expr2) -> (
        let t1 = check expr1 in
        let t2 = check expr2 in
        match bop with
        | Add ->
            expect_ty3 TInt t1 t2;
            TInt)
  in
  check expr

let rec check_stmt env stmt =
  match stmt with
  | SVar (name, typing, expr) ->
      if mem name env.venv then
        raise (TypeError ("duplicate symbol definition: " ^ name))
      else
        let ty = check_typing env typing in
        let expr_ty = check_expr env expr in
        expect_ty ty expr_ty;
        { env with venv = (name, ty) :: env.venv }
  | SExpr expr ->
      let _ = check_expr env expr in
      env
  | SIfElse (expr, block1, block2) ->
      let expr_ty = check_expr env expr in
      expect_ty TBool expr_ty;
      let _ = check_block env block1 in
      let _ = check_block env block2 in
      env
  | SFor (name, expr1, expr2, block) ->
      if mem name env.venv then
        raise (TypeError ("duplicate symbol definition: " ^ name))
      else (
        expect_ty TInt (check_expr env expr1);
        expect_ty TInt (check_expr env expr2);
        let env' = { env with venv = (name, TInt) :: env.venv } in
        let _ = check_block env' block in
        env)

and check_block env (Block stmts) = List.fold_left check_stmt env stmts

let check (Program stmts) =
  let base_env = { tenv = base_tenv; venv = base_venv } in
  let _ = check_block base_env stmts in
  ()
