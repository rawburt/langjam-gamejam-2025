type ty =
  | TyNull
  | TyUnit
  | TyBool
  | TyInt
  | TyColor
  | TyStr
  | TyImage
  | TyFun of ty list * ty
  | TyList of ty
  | TyVar of ty option ref
  | TyVarNamed of string
  | TyRec of (string * ty) list
  | TyEnum of string * string list
  | TyOpt of ty
  | TyUnion of ty list

let rec show_ty = function
  | TyNull -> "null"
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
      match !tyoptref with Some t -> show_ty t | None -> "_")
  | TyVarNamed name -> "'" ^ name
  | TyRec fields ->
      let field_ty (name, ty) = Printf.sprintf "%s: %s" name (show_ty ty) in
      let inner = List.map field_ty fields |> String.concat ", " in
      Printf.sprintf "rec(%s)" inner
  | TyEnum (name, _) -> Printf.sprintf "enum(%s)" name
  | TyOpt ty -> show_ty ty ^ "?"
  | TyUnion tys ->
      let inner = List.map show_ty tys |> String.concat " | " in
      Printf.sprintf "(%s)" inner
