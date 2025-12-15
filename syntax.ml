type loc = Loc of int [@@deriving show]
type typing = TName of string | TList of typing [@@deriving show]
type bop = Add | Sub | Mul | Eq [@@deriving show]

type var = VName of string * loc | VSub of var * expr * loc [@@deriving show]

and expr =
  | EBool of bool
  | EInt of int
  | EColor of string
  | EVar of var
  | ECall of var * expr list * loc
  | EBinary of bop * expr * expr * loc
  | EList of expr list * loc
[@@deriving show]

type stmt =
  | SVar of string * typing * expr * loc
  | SMutate of var * expr * loc
  | SExpr of expr * loc
  | SIfElse of expr * block * block option * loc
  | SFor of string * expr * expr * block * loc
[@@deriving show]

and block = Block of stmt list [@@deriving show]

type def = {
  name : string;
  params : (string * typing) list;
  body : block;
  loc : loc;
}
[@@deriving show]

type toplevel = TLStmt of stmt | TLDef of def [@@deriving show]
type program = Program of toplevel list [@@deriving show]
