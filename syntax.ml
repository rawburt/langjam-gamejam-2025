type loc = Loc of string * int [@@deriving show]
type ident = string * int [@@deriving show]

let loc_get_file (Loc (f, _)) = f
let mkloc (s : Lexing.position) = Loc (s.pos_fname, s.pos_lnum)

type typing =
  | TName of ident * loc
  | TList of typing * loc
  | TOpt of typing * loc
  | TParam of ident * loc
  | TUnion of typing list * loc
[@@deriving show]

type bop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Lt
  | Gt
  | Eq
  | Neq
  | Lte
  | Gte
  | Or
  | And
[@@deriving show]

type uop = Minus | Negate [@@deriving show]

type var =
  | VName of ident * loc
  | VSub of var * expr * loc
  | VField of var * string * loc
[@@deriving show]

and const =
  | CNull
  | CInt of int
  | CBool of bool
  | CStr of string
  | CColor of string
[@@deriving show]

and expr =
  | EConst of const
  | EVar of var
  | ECall of var * expr list * loc
  | EBinary of bop * expr * expr * loc
  | EUnary of uop * expr * loc
  | EList of expr list * loc
  | ERec of ident * (string * expr) list * loc
  | EEnum of ident * string * loc
  | ESafeBind of ident * expr * loc
[@@deriving show]

type stmt =
  | SVar of ident * typing * expr * loc
  | SMutate of var * expr * loc
  | SExpr of expr * loc
  | SIfElse of expr * block * block option * loc
  | SFor of ident * expr * expr * block * loc
  | SForIn of ident * expr * block * loc
  | SRet of expr * loc
  | SMatch of expr * (expr * block) list * loc
  | SBreak of loc
  | SCond of (expr * block) list * loc
[@@deriving show]

and block = Block of stmt list [@@deriving show]

type def = {
  name : ident;
  type_params : ident list;
  params : (ident * typing) list;
  body : block;
  ret : typing option;
  ffi : string option;
  loc : loc;
}
[@@deriving show]

type record = { name : ident; fields : (string * typing) list; loc : loc }
[@@deriving show]

type toplevel =
  | TLStmt of stmt
  | TLDef of def
  | TLRec of record
  | TLLoad of ident * string * loc
  | TLConst of ident * typing * expr * loc
  | TLEnum of ident * string list * loc
[@@deriving show]

type library =
  | Library of { top : toplevel list; imports : (string * loc) list }
[@@deriving show]

type program = Program of toplevel list [@@deriving show]
