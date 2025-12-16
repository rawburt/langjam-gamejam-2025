type loc = Loc of string * int [@@deriving show]

let mkloc (s : Lexing.position) = Loc (s.pos_fname, s.pos_lnum)

type typing = TName of string | TList of typing [@@deriving show]
type bop = Add | Sub | Mul | Lt | Gt | Eq | Or | And [@@deriving show]

type var =
  | VName of string * loc
  | VSub of var * expr * loc
  | VField of var * string * loc
[@@deriving show]

and expr =
  | EBool of bool
  | EInt of int
  | EColor of string
  | EStr of string
  | EVar of var
  | ECall of var * expr list * loc
  | EBinary of bop * expr * expr * loc
  | EList of expr list * loc
  | ERec of string * (string * expr) list * loc
[@@deriving show]

type stmt =
  | SVar of string * typing * expr * loc
  | SMutate of var * expr * loc
  | SExpr of expr * loc
  | SIfElse of expr * block * block option * loc
  | SFor of string * expr * expr * block * loc
  | SRet of expr * loc
[@@deriving show]

and block = Block of stmt list [@@deriving show]

type def = {
  name : string;
  params : (string * typing) list;
  body : block;
  ret : typing option;
  loc : loc;
}
[@@deriving show]

type record = { name : string; fields : (string * typing) list; loc : loc }
[@@deriving show]

type toplevel = TLStmt of stmt | TLDef of def | TLRec of record
[@@deriving show]

type library = Library of { top : toplevel list; imports : (string * loc) list }
[@@deriving show]

type program = Program of toplevel list [@@deriving show]
