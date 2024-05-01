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
  weight: int; 
}

type graph_element =
  | Vertex of string
  | Edge of string * string * int

type expr =
  | Lit of int
  | FloatLit of float 
  | BoolLit of bool
  | Var of string
  | Graph of graph_element list
  | Uniop of uniop * expr
  | Binop of expr * binop * expr
  | Seq of expr * expr
  | GraphAsn of string * expr
  | Asn of string * expr

 
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
  | Uniop(op, e) ->
    let op_str = match op with
      | Not -> "!"
      | Dot -> "."
    in
    op_str ^ string_of_expr e
  | Graph(elements) ->
    "Graph([" ^ String.concat ", " (List.map string_of_graph_element elements) ^ "])"
  | GraphAsn(v, elt_list) -> "GraphAsn: " ^ v  ^ "<elt_list goes here>"
  | Seq(e1, e2) -> string_of_expr e1 ^ "; " ^ string_of_expr e2

 and string_of_graph_element = function
  | Vertex(vertex) -> "vertex:" ^ vertex
  | Edge(n1, n2, weight) ->  "source:" ^ n1  ^ ", dest: " ^ n2 ^ ", weight:" ^ string_of_int(weight)
  
and string_of_vertex vertex =
  "\"" ^ vertex ^ "\""
