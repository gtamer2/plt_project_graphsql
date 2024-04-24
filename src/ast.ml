
type binop = Add | Sub | Mul | Div | Mod 
type uniop = Not | Dot
type typ = Int | Bool | Float | String | Void 
type boolop = Eq | Neq | Gteq | Lteq | Gt | Lt | And | Or

(* type vertex = {
  id: string;
}

type edge = {
  source: string;
  target: string;
  weight: float; (* Use option type for optional weight, assuming weighted edges *)
}

type graph_element =
  | Vertex of vertex
  | Edge of edge *)

type expr =
  | Binop of expr * binop * expr
  | Seq of expr * expr
  | Asn of string * expr
  | Var of string
  | Lit of int
  | FloatLit of float 
  | BoolLit of bool
  | Bool_Binop of expr * boolop * expr
  (* | Graph of graph_element list * graph_element list   *)
  (* | NamedGraph of string * (graph_element list) * (graph_element list) *)
