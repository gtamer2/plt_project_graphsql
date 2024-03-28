type bigraphoperator = Union | Intersect

type expr =
    BinGraphOp of expr * bigraphoperator * expr
  | Var of string

(*  type operator = Add | Sub | Mul | Div
    type expr =
    Binop of expr * operator * expr
  | Lit of int
  | Seq of expr * expr
  | Asn of string * expr
  | Var of string *)
