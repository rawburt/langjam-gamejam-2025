open Syntax

type ty = TUnit | TInt | TColor | TFun of ty list * ty

exception TypeError

let expect_tys ts1 ts2 = if ts1 = ts2 then () else raise TypeError

let rec check_expr = function
  | EInt _ -> TInt
  | EColor _ -> TColor
  | ECall (name, exprs) -> (
      let expr_tys = List.map check_expr exprs in
      match name with
      | "pset" ->
          expect_tys [ TInt; TInt; TColor ] expr_tys;
          TUnit
      | _ -> failwith "check_expr: ECall: todo")

let check (Program exprs) =
  let _ = List.map check_expr exprs in
  ()
