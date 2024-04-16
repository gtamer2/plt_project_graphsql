type binop = Add | Sub | Mult | Divd | Mod | Eq | Neq | Gteq | Lteq | Gt | Lt | And | Or

type uniop = Not | Dot

type typ = Int | Bool | Float | String | Void

type expr =
    Variable of string
  | Literal of int
  | BoolLit of bool
  | FloatLit of float
  | StringLit of string
  | Assign of string * expr
  | Binop of expr * binop * expr
  (*| Call of string * expr list *)
  | Sequence of expr * expr