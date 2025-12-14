type expr = EInt of int | EColor of string | ECall of string * expr list
[@@deriving show]

type program = Program of expr list [@@deriving show]
