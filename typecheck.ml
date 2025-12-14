open Syntax

type ty = TUnit | TBool | TInt | TColor | TFun of ty list * ty
[@@deriving show]

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
  if ts1 = ts2 then ()
  else
    let s1 = List.map show_ty ts1 |> String.concat ", " in
    let s2 = List.map show_ty ts2 |> String.concat ", " in
    raise (TypeError (Printf.sprintf "types mismatched (list):\n%s\n%s" s1 s2))

let expect_ty t1 t2 =
  if t1 = t2 then ()
  else
    let s1 = show_ty t1 in
    let s2 = show_ty t2 in
    raise (TypeError (Printf.sprintf "type mismatch (2):\n%s\n%s" s1 s2))

let expect_ty3 t1 t2 t3 =
  if t1 = t2 && t1 = t3 then ()
  else
    let s1 = show_ty t1 in
    let s2 = show_ty t2 in
    let s3 = show_ty t3 in
    raise (TypeError (Printf.sprintf "type mismatch (3):\n%s\n%s\n%s" s1 s2 s3))

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
  | SMutate (name, expr) ->
      if mem name env.venv then (
        let ty = lookup name env.venv in
        let expr_ty = check_expr env expr in
        expect_ty ty expr_ty;
        env)
      else raise (TypeError ("symbol not declared: " ^ name))
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

let check_toplevel env = function
  | TLStmt stmt -> check_stmt env stmt
  | TLDef def ->
      if mem def.name env.venv then
        raise (TypeError ("duplicate symbol definition: " ^ def.name))
      else
        let params =
          List.map
            (fun (name, typing) -> (name, check_typing env typing))
            def.params
        in
        let env' = { env with venv = params @ env.venv } in
        let _ = check_block env' def.body in
        let tys = List.map snd params in
        { env with venv = (def.name, TFun (tys, TUnit)) :: env.venv }

let check (Program toplevels) =
  let base_env = { tenv = base_tenv; venv = base_venv } in
  let _ = List.fold_left check_toplevel base_env toplevels in
  ()
