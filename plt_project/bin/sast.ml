(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast  

(* TYPES - START *)
type graph_element_type =
  | VertexType
  | EdgeType

type unified_type = 
  | Int
  | Bool
  | Float
  | String
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


type vdecl = unified_type * string

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
  | SGraphAsn of string * sx
  | SFunctionCall of string * sexpr list
  | Return of sexpr

type sstmt = 
  | SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt list
  | SIfElse of sexpr * sstmt list * sstmt list
  | SWhile of sexpr * sstmt list
  | SFor of sexpr * sexpr * sexpr * sstmt list
  | SFunctionCreation of string * vdecl list * sstmt list * unified_type

let get_graph_sx sgraphexpr = 
  match sgraphexpr with 
  | SGraph sgraph_elements_list -> sgraph_elements_list
  | _ -> []

let string_of_typ t = 
  begin match t with
  | Int -> "Int"
  | Bool -> "Bool"
  | Float -> "Float"
  | String -> "String"
  | GraphType gts -> 
      "GraphType[" ^ String.concat ", " (List.map (function
        | VertexType -> "Vertex"
        | EdgeType -> "Edge") gts) ^ "]"
  end

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (begin match e with
    | SLit(l) -> string_of_int l
    | SBoolLit(b) -> string_of_bool b
    | SFloatLit(f) -> string_of_float f
    | SVar(s) -> s
    | SUniop(op, e1) ->
      let op_str = "!" in
        "(" ^ op_str ^ " " ^ string_of_sexpr e1 ^ ")"
    | SAsn(gname, sgraph) -> gname ^ " = " ^ string_of_sexpr sgraph
    | SBinop(e1, op, e2) ->
        let op_str = match op with
          | Add -> "+"
          | Sub -> "-"
          | Mul -> "*"
          | Div -> "/"
          | Eq -> "=="
          | Neq -> "!="
          | Gteq -> ">="
          | Lteq -> "<="
          | Gt -> ">"
          | Lt -> "<"
          | And -> "&&"
          | Or -> "||"
        in
        string_of_sexpr e1 ^ " " ^ op_str ^ " " ^ string_of_sexpr e2
    | SGraph(elements) ->
        "Graph([" ^ String.concat ", " (List.map string_of_sgraph_element elements) ^ "])"
    | SGraphAsn(gname, sgraph) -> "GraphAsn: " ^ gname ^ ", Graph([" ^ String.concat ", " (List.map string_of_sgraph_element (get_graph_sx sgraph)) ^ "])"
    | SFunctionCall(fname, args) -> "FunctionCall: " ^ fname ^ "(" ^ String.concat ", " (List.map string_of_sexpr args) ^ ")"
    end
  ) ^ ")"

and string_of_sgraph_element = function
  | (VertexType, SVertex { sid }) -> "Vertex(" ^ sid ^ ")"
  | (EdgeType, SEdge { ssource; starget; sweight }) ->
      "Edge(" ^ ssource ^ ", " ^ starget ^ ", " ^ string_of_int sweight ^ ")"

and string_of_svertex svertex =
  "\"" ^ svertex.sid ^ "\""
and string_of_sedge sedge =
  let weight_str = match sedge.sweight with
    | w -> string_of_int w
    | _ -> "no weight"
  in
  "Edge(\"" ^ sedge.ssource ^ "\", \"" ^ sedge.starget ^ "\", " ^ weight_str ^ ")"

and string_of_sstmt = function
    SBlock(stmt_list) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmt_list) ^ "}\n"
  | SExpr (sexpr) -> string_of_sexpr sexpr ^ ";\n"
  | SIf (sexpr, sstmt_list) -> string_of_sexpr sexpr ^ "{\n" ^ String.concat "" (List.map string_of_sstmt sstmt_list) ^ "}\n"
  | SIfElse (sexpr, sstmt_list_1, sstmt_list_2) -> string_of_sexpr sexpr ^ "{\n" ^ String.concat "" (List.map string_of_sstmt sstmt_list_2) ^ "}\n" ^ "{\n" ^ String.concat "" (List.map string_of_sstmt sstmt_list_2) ^ "}\n"
  | SWhile (sexpr, sstmt_list) -> string_of_sexpr sexpr ^ "{\n" ^ String.concat "" (List.map string_of_sstmt sstmt_list) ^ "}\n"
  | SFor (sexpr1, sexpr2, sexpr3, sstmt_list) -> string_of_sexpr sexpr1 ^ string_of_sexpr sexpr2 ^ string_of_sexpr sexpr3 ^ "{\n" ^ String.concat "" (List.map string_of_sstmt sstmt_list) ^ "}\n"
  | SFunctionCreation (fname, fargs, sstmt_list, return_type) -> "Function " ^ fname ^ " Function args TODO"
    

  