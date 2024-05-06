(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast  

(* TYPES - START *)
type primitive_typ =
  | Int
  | Bool
  | Float
  | String


type graph_element_type =
  | VertexType
  | EdgeType

type unified_type = 
  | Typ of primitive_typ
  | GraphElementType of graph_element_type
  | GraphType of graph_element_type list

(* SEMANTIC AST - START *)
type svertex = {
  sid: string;
}
  
type sedge = {
  ssource: string;
  starget: string;
  sweight: int; 
}

type sgraph_element = graph_element_type * sgraph_element_x
and sgraph_element_x =
  | SVertex of svertex
  | SEdge of sedge

(* type sgraph_type =
 | graph_element list

type sgraph = sgraph_type * sgraph_x 
and sgraph_x = 
 | S
   *)

type sexpr = unified_type * sx
and sx = 
    SLit of int
  | SFloatLit of float
  | SBoolLit of bool
  | SVar of string
  | SAsn of string * sexpr
  | SUniop of uniop * sexpr
  | SBinop of sexpr * binop * sexpr
  | SGraph of sgraph_element list
  (* | SGraphAccess of string * string *)
  | SGraphAsn of string * sexpr
  (* | SGraphOp of string * sgraph_element list * string
  | SGraphQuery of string * string * string
  | SGraphUpdate of string * graph_element *)

type sstmt = 
  | SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt list
  | SIfElse of sexpr * sstmt list * sstmt list
  | SWhile of sexpr * sstmt list
  | SFor of sexpr * sexpr * sexpr * sstmt list

(* let rec string_of_sexpr (t, e) = match e
  | SLit(l[0],l[1]) -> string_of_int l *)

let string_of_typ t = 
  match t with
  | Int -> "Int"
  | Bool -> "Bool"
  | Float -> "Float"
  | String -> "String"
  | GraphType gt -> 
      match gt with
      | VertexType -> "Vertex"
      | EdgeType -> "Edge"

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
        SLit(l) -> string_of_int l
      | SBoolLit(true) -> "true"
      | SBoolLit(false) -> "false"
      | SFloatLit(f) -> string_of_float f
      | SVar(s) -> s
      | SBinop(e1, op, e2) ->
        let op_str = string_of_op op
        in
        "(" ^ string_of_sexpr e1 ^ " " ^ op_str ^ " " ^ string_of_sexpr e2 ^ ")"
      | SAsn(p, q) -> p ^ " = " ^ string_of_sexpr q
      | SGraph(elements) ->
        "Graph([" ^ String.concat ", " (List.map string_of_sgraph_element elements) ^ "])"
      | SGraphAsn(p, q) -> "GraphAsn: " ^ p  ^ string_of_sexpr q
      ) ^ ")"

and string_of_sgraph_element (t , e) =
  "(" ^ string_of_graph_element t ^ ":"  ^ ( match e with
    SVertex(svertex) -> string_of_svertex svertex
  | SEdge(sedge) -> string_of_sedge sedge
  ) ^ ")"

and string_of_svertex svertex =
  "\"" ^ svertex.sid ^ "\""
and string_of_sedge sedge =
  let weight_str = match sedge.sweight with
    | Some w -> string_of_int w
    | None -> "None"
  in
  "Edge(\"" ^ sedge.ssource ^ "\", \"" ^ sedge.starget ^ "\", " ^ weight_str ^ ")"

and string_of_sstmt = function
    SBlock(stmt_list) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmt_list) ^ "}\n"
  | SExpr (sexpr) -> string_of_sexpr sexpr ^ ";\n"

    

  