type bigraphoperator = Union | Intersect

(* type CreateGraph  *)
(* type expr =
graph name, list of vertixes, list of edges)
    CreateGraph of Lit * list[int] * Edge
    BinGraphOp of expr * bigraphoperator * expr
  | Var of string *)




  
(*  type operator = Add | Sub | Mul | Div
    type expr =
    Binop of expr * operator * expr
  | Lit of int
  | Seq of expr * expr
  | Asn of string * expr
  | Var of string *)
