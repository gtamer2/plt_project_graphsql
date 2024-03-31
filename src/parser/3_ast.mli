

type arithbinop = Add | Sub | Mult | Divd | Mod | Eq | Neq | Gteq | Steq | Gt | St

type bigraphoperator = Union | Intersect

type typ = Int | Bool | Float | String

type graphdirect = Undirected | Directed

type graphweight = Unweighted | Weighted

type vertex = 
  Prim of typ

type edge = {
  vertex1: vertex;
  vertex2: vertex;
  direct_type: graphdirect;
  weight_type: graphweight;
}

type graph = {
  vertices: vertex list;
  edges: edge list;
  direct_type: graphdirect;
  weight_type: graphweight;
}

type expr =
    Variable of string
  | Assign of string * expr
  | Literal of int

type stmt =
  Expr of expr
  (* | GraphExpr of graph_expr *)

type program = stmt list





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
