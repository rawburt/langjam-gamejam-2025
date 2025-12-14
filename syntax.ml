type typing = TName of string [@@deriving show]
type var = VName of string [@@deriving show]
type bop = Add [@@deriving show]

type expr =
  | EBool of bool
  | EInt of int
  | EColor of string
  | EVar of var
  | ECall of var * expr list
  | EBinary of bop * expr * expr
[@@deriving show]

type stmt =
  | SVar of string * typing * expr
  | SExpr of expr
  | SIfElse of expr * block * block
  | SFor of string * expr * expr * block
[@@deriving show]

and block = Block of stmt list [@@deriving show]

type program = Program of block [@@deriving show]
