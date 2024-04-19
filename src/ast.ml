
type binop = Add | Sub | Mul | Div | Mod | Eq | Neq | Gteq | Lteq | Gt | Lt | And | Or
type uniop = Not | Dot
(* type primitive = Int | Bool | Float | String | Void 
type object = Graph *)

type vertex = {
  id: string;
}

type edge = {
  source: string;
  target: string;
  weight: float; (* Use option type for optional weight, assuming weighted edges *)
}

type graph_element =
  | Vertex of vertex
  | Edge of edge

(* type assignment =
  | Asn of string * primitive
  | Asn of string * Graph *)

type expr =
  | Binop of expr * binop * expr
  | Seq of expr * expr
  (* | Asn of string * Graph
  | Asn of string * primitive *)
  | Asn of string * expr
  | Var of strings
  | Lit of int
  | FloatLit of float 
  | BoolLit of bool
  | Graph of graph_element list * graph_element list  
  (* | Graph of graph_element list * graph_element list   *)
  (* | NamedGraph of string * (graph_element list) * (graph_element list) *)

let rec string_of_expr = function
  | Lit(l) -> string_of_int l
  | FloatLit(f) -> string_of_float f
  | BoolLit(b) -> string_of_bool b
  | Var(v) -> v
  | Asn(v, e) -> v ^ " = " ^ string_of_expr e
  | Binop(e1, op, e2) -> 
    let op_str = match op with
      | Add -> "+"
      | Sub -> "-"
      | Mul -> "*"
      | Div -> "/"
      | Mod -> "%"
      | Eq -> "=="
      | Neq -> "!="
      | Gteq -> ">="
      | Lteq -> "<="
      | Gt -> ">"
      | Lt -> "<"
      | And -> "&&"
      | Or -> "||"
    in
    "(" ^ string_of_expr e1 ^ " " ^ op_str ^ " " ^ string_of_expr e2 ^ ")"
  | Graph(vertices, edges) ->
      "Graph([" ^ String.concat ", " (List.map string_of_graph_element vertices) ^
      "], [" ^ String.concat ", " (List.map string_of_graph_element edges) ^ "])"
and string_of_graph_element = function
  | Vertex(vertex) -> string_of_vertex vertex
  | Edge(edge) -> string_of_edge edge
and string_of_vertex vertex =
  "\"" ^ vertex.id ^ "\""
and string_of_edge edge =
  let weight_str = string_of_float edge.weight in
  "Edge(\"" ^ edge.source ^ "\", \"" ^ edge.target ^ "\", " ^ weight_str ^ ")"