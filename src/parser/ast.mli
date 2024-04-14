type binop = Add | Sub | Mult | Divd | Mod | Eq | Neq | Gteq | Lteq | Gt | Lt | And | Or

type uniop = Not | Dot

type bigraphoperator = Union | Intersect

type typ = Int | Bool | Float | String | Void

type graphdirect = Undirected | Directed

type graphweight = 
  Unweighted 
  | Weighted of int

type vertex = 
    VerLiteral of int
  | VerBoolLit of bool
  | VerFloatLit of float
  | VerStringLit of string

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
  | Literal of int
  | BoolLit of bool
  | FloatLit of float
  | Assign of string * expr
  | Binop of expr * binop * expr
  | Call of string * expr list
  | Seq of expr * expr

  type bind = typ * expr

type graph_expr =
    Vertex of vertex
  | Edge of edge
  | Graph of graph

type graph_stmt = 
    Create of string * graph_expr
  | Insert of string * graph_expr
  | Delete of string * graph_expr
  | Update of string * graph_expr

type stmt =
  Expr of expr
  | Block of stmt list
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Return of expr
  | GraphStmt of graph_stmt

type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
}

type program = stmt_list (*bind list * stmt list * func_def list*)




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

(* pretty printer functions *)

