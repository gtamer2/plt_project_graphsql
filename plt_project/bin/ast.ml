type binop = Add | Sub | Mul | Div | Mod | Eq | Neq | Gteq | Lteq | Gt | Lt | And | Or
type uniop = Not | Dot
type typ = Int | Bool | Float | String (* | Void *)
(* type object = Graph *)


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

type unified_type = 
  | Typ of typ
  | GraphElement of graph_element
  | Graph of graph_element list

type expr =
  | Lit of int
  | FloatLit of float 
  | BoolLit of bool
  | Var of string
  | Asn of string * expr 
  | Uniop of uniop * expr
  | Binop of expr * binop * expr
  | Seq of expr * expr
  | Graph of graph_element list
  | GraphAccess of string * string (* graph_name * field_name *)
  | GraphAsn of string * expr
  | If of expr * expr
  | IfElse of expr * expr * expr
  (* | AccessResult of graph_element list *)

let rec string_of_expr = function
  | Lit(l) -> string_of_int l
  | FloatLit(f) -> string_of_float f
  | BoolLit(b) -> string_of_bool b
  | Var(v) -> v
  | Asn(v, e) -> v ^ " = " ^ string_of_expr e
  (* | Asn(v, e) -> v ^ " = " ^ string_of_expr e *)
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
  | GraphAccess(graphname, fieldname) -> "GraphAccessing... graphname:" ^ graphname ^ ", fieldname:" ^ fieldname
  | GraphAsn(v, elt_list) -> 
     "GraphAsn: " ^ v  ^ "TODO print all elements "
    (* let elements_str = List.map string_of_expr elt_list |> String.concat ", " in *)
    (* "GraphAsn " ^ v ^ (List.map string_of_graph_element elt_list) ^ "])" *)
  | Seq(e1, e2) -> string_of_expr e1 ^ "; " ^ string_of_expr e2
  | If(condition, body) -> "IF(" ^ string_of_expr condition ^ ") THEN " ^ string_of_expr body
  | IfElse(condition, truebody, elsebody) -> "IF(" ^ string_of_expr condition ^ ") THEN " ^ string_of_expr truebody ^ " ELSE " ^ string_of_expr elsebody

and string_of_graph_element = function
  | Vertex(vertex) -> "vertex:" ^ vertex
  | Edge(n1, n2, weight) ->  "source:" ^ n1  ^ ", dest: " ^ n2 ^ ", weight:" ^ string_of_int(weight)
  
and string_of_vertex vertex =
  "\"" ^ vertex ^ "\""
