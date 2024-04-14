type binop = Add | Sub | Mult | Divd | Mod | Eq | Neq | Gteq | Lteq | Gt | Lt | And | Or

type uniop = Not | Dot

(* type bigraphoperator = Union | Intersect *)

type typ = Int | Bool | Float | String | Void

(* type graphdirect = Undirected | Directed

type graphweight = 
  Unweighted (* set to 1? *)
  | Weighted of int

type vertex = 
    VerLiteral of int
  | VerBoolLit of bool
  | VerFloatLit of float
  | VerStringLit of string

type edge = {
  vertex1: vertex;
  vertex2: vertex;
  is_directed: graphdirect;
  is_weighted: graphweight;
}

type graph = {
  vertices: vertex list;
  edges: edge list;
  direct_type: graphdirect;
  weight_type: graphweight;
} *)

type expr =
    Variable of string
  | Literal of int
  | BoolLit of bool
  | FloatLit of float
  | StringLit of string
  | Assign of string * expr
  | Binop of expr * binop * expr
  (*| Call of string * expr list *)
  | Seq of expr * expr

(*type bind = typ * expr *)

(* type graph_expr =
    Vertex of vertex
  | Edge of edge
  | Graph of graph *)

  (* definitiely more than just string * graph_expr *)
(* type graph_stmt = 
    Create of string * graph_expr
  | Insert of string * graph_expr
  | Delete of string * graph_expr
  | Update of string * graph_expr *)

(*type stmt =
  Expr of expr
  | Block of stmt list
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Return of expr*)
  (* | GraphStmt of graph_stmt *)

(* type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
} *)

(* very place holder, maybe just stmt list? *)
(*type program = expr*)
(* type program = bind list * stmt list * func_def list *)





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


(*let rec string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Divd -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Gteq -> ">="
  | Lteq -> "<="
  | Gt -> ">"
  | Lt -> "<"
  | And -> "AND"
  | Or -> "OR"*)

(* let rec string_of_vertex = function
  VerLiteral(l) -> string_of_int l
  | VerBoolLit(true) -> "True"
  | VerBoolLit(false) -> "False"
  | VerFloatLit(f) -> string_of_float f
  | VerStringLit(s) -> "\"" ^ s ^ "\""

let rec string_of_edge edge = match edge.weight_type, edge.direct_type with
  | Unweighted, Undirected -> 
    "EDGE(" ^ string_of_vertex edge.vertex1 ^ "-" ^ string_of_vertex edge.vertex2 ^ ")"
  | Weighted(w), Undirected  -> 
    "EDGE(" ^ string_of_vertex edge.vertex1 ^ "-" ^ string_of_vertex edge.vertex2 ^ "," ^ string_of_int w ^ ")"
  | Unweighted, Directed -> 
    "EDGE(" ^ string_of_vertex edge.vertex1 ^ "->" ^ string_of_vertex edge.vertex2 ^ ")"
  | Weighted(w), Directed  -> 
    "EDGE(" ^ string_of_vertex edge.vertex1 ^ "->" ^ string_of_vertex edge.vertex2 ^ "," ^ string_of_int w ^ ")" *)

(*let rec string_of_expr = function
  Variable(s) -> s
  | Literal(l) -> string_of_int l
  | BoolLit(true) -> "True"
  | BoolLit(false) -> "False"
  | FloatLit(f) -> string_of_float f
  | StringLit(s) -> "\"" ^ s ^ "\""
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_binop o ^ " " ^ string_of_expr e2
  | Call(f, el) -> 
    "APPLY(" ^ f ^ "," ^ String.concat "," (List.map string_of_expr el) ^ ")" 
*)


(*let rec string_of_stmt = function
  Expr(e) -> string_of_expr e
  | Block(stmts) -> 
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | If(e, s1, s2) -> 
    "IF " ^ string_of_expr e ^ "\n" ^
    string_of_stmt s1 ^ "ELSE\n" ^ string_of_stmt s2
  | While(e, s) -> "WHILE " ^ string_of_expr e ^ " " ^ string_of_stmt s
  | Return(expr) -> "RETURN " ^ string_of_expr expr ^ ";\n"
  | GraphStmt(stmt) -> string_of *)