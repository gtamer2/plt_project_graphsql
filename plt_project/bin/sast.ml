(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast  

type svertex = {
  sid: string;
}

type sedge = {
  ssource: string;
  starget: string;
  sweight: float option; 
}

type sgraph_element = graph_element * graph_element_x
and graph_element_x =
  | SVertex of svertex
  | SEdge of sedge

type sexpr = typ * sx
and sx = 
    SLit of int
  | SFloatLit of float
  | SBoolLit of bool
  | SVar of string
  | SAsn of string * sexpr
  | SUniop of uniop * sexpr
  | SBinop of sexpr * binop * sexpr
  | SSeq of sexpr * sexpr
  | SGraph of sgraph_element list
  | SGraphAccess of string * string
  | SGraphAsn of string * sexpr
  | SIf of sexpr * sexpr
  | SIfElse of sexpr * sexpr * sexpr

(* let rec string_of_sexpr (t, e) = match e
  | SLit(l[0],l[1]) -> string_of_int l *)

let string_of_typ t = match t with
  Int -> "Int"
  | Bool -> "Bool"
  | Float -> "Float"
  | String -> "String"
  (* | Vertex -> "Vertex"
  | Edge -> "Edge" *)

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
        SLit(l) -> string_of_int l
      | SBoolLit(true) -> "true"
      | SBoolLit(false) -> "false"
      | SFloatLit(f) -> string_of_float f
      | SVar(s) -> s
      | SBinop(e1, op, e2) ->
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
        "(" ^ string_of_sexpr e1 ^ " " ^ op_str ^ " " ^ string_of_sexpr e2 ^ ")"
      | SAsn(p, q) -> p ^ " = " ^ string_of_sexpr q
      | SGraph(elements) ->
        "Graph([" ^ String.concat ", " (List.map string_of_sgraph_element elements) ^ "])"
      | SGraphAsn(p, q) -> "GraphAsn: " ^ p  ^ string_of_sexpr q
      | SSeq(e1, e2) -> string_of_sexpr e1 ^ "; " ^ string_of_sexpr e2
        
  )

and string_of_sgraph_element (t , e) =
  "(" ^ string_of_graph_element t ^ ":"  ^ ( match e with
    SVertex(svertex) -> string_of_svertex svertex
  | SEdge(sedge) -> string_of_sedge sedge
  )

and string_of_svertex svertex =
  "\"" ^ svertex.sid ^ "\""
and string_of_sedge sedge =
  let weight_str = match sedge.sweight with
    | Some w -> string_of_float w
    | None -> "None"
  in
  "Edge(\"" ^ sedge.ssource ^ "\", \"" ^ sedge.starget ^ "\", " ^ weight_str ^ ")"
  