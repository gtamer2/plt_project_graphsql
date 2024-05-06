type binop = Add | Sub | Mul | Div | Mod | Eq | Neq | Gteq | Lteq | Gt | Lt | And | Or
type uniop = Not | Dot
(* type typ = Int | Bool | Float | StringType  *)


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
  (* | Typ of typ *)
  | Int
  | Bool 
  | Float
  | StringType
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
  (* | Seq of expr * expr *)
  | Graph of graph_element list
  | GraphAsn of string * expr
  | GraphAccess of string * string (* graph_name * field_name *)
  | GraphOp of string * graph_element list * string
  | GraphQuery of string * string * string
  | GraphUpdate of string * graph_element
  | If of expr * expr
  | IfElse of expr * expr * expr

type stmt = 
  | Block of stmt list
  | Expr of expr
  | If of expr * stmt list
  | IfElse of expr * stmt list * stmt list
  | IfElif of expr * stmt list * elif_stmt list * stmt list
  | While of expr * stmt list
  | For of expr * expr * expr * stmt list
  and
 elif_stmt = expr * stmt list


type stmt_list = stmt list 



let rec string_of_expr = function
  | Lit(l) -> string_of_int l
  | FloatLit(f) -> string_of_float f
  | BoolLit(b) -> string_of_bool b
  | Var(v) -> v
  | Asn(v, e) -> v ^ " = " ^ string_of_expr e
  | Binop(e1, op, e2) ->
    let op_str = string_of_op op
    in
    "(" ^ string_of_expr e1 ^ " " ^ op_str ^ " " ^ string_of_expr e2 ^ ")"
  | Uniop(op, e) ->
    let op_str = match op with
      | Not -> "!"
      | Dot -> "."
    in
    op_str ^ string_of_expr e
  | Graph(elements) ->
    "\n" ^ "Graph([" ^ String.concat ", " (List.map string_of_graph_element elements) ^ "])"
  | GraphAccess(graphname, fieldname) -> "\n" ^ "GraphAccessing... graphname:" ^ graphname ^ ", fieldname:" ^ fieldname
  | GraphAsn(v, elt_list) -> 
    (* "\n" ^ "GraphAsn: " ^ v  ^ "TODO print all elements "  *)
    "\n" ^ "GraphAsn: " ^ v  ^ "[" ^ string_of_expr elt_list ^ "]"
    (* "\n" ^ "GraphAsn: " ^ v  ^ "[" ^ String.concat ", " (List.map string_of_expr elt_list) ^ "]" *)
  | GraphQuery(gname1, gname2, queryType) ->
    "\n GraphQuerying..." ^ gname1 ^ queryType ^ gname2
  | GraphOp(gname, elements, optype) -> 
    "\n" ^ "Graph:" ^ gname ^ "[" ^ String.concat ", " (List.map string_of_graph_element elements) ^ "]" ^ "OpType:" ^ optype
  | GraphUpdate(gname, element) ->
    "\n Updating graph element" ^ string_of_graph_element element ^ "in graph: " ^ gname  

and string_of_graph_element = function
| Vertex(vertex) -> "vertex:" ^ vertex
| Edge(n1, n2, weight) ->  "source:" ^ n1  ^ ", dest: " ^ n2 ^ ", weight:" ^ string_of_int(weight)

and string_of_vertex vertex =
  "\"" ^ vertex ^ "\""

(* convert op to string separated out as a separate function *)
and string_of_op op = match op with
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



let rec string_of_stmt = function
  | If(condition, body) -> "\n" ^ "IF(" ^ string_of_expr condition ^ ") THEN " ^ string_of_stmt_list body
  | IfElse(condition, truebody, elsebody) -> "\n" ^ "IF(" ^ string_of_expr condition ^ ") THEN " ^ string_of_stmt_list truebody ^ " ELSE " ^ string_of_stmt_list elsebody
  | While(condition, body) -> "WHILE(" ^ string_of_expr condition ^ ") DO " ^ string_of_stmt_list body
  | Expr(expr) -> string_of_expr expr ^ " "
  | Block(stmts) -> "TODO BLOCK " 
  | For(init, condition, increment, body) -> "FOR (" ^ string_of_expr init ^ "; " ^ string_of_expr condition ^ "; " ^ (string_of_expr increment) ^ ") {" ^ string_of_stmt_list  body ^ "}"
  | IfElif(condition, truebody, eliflist, elsebody) -> "\nIF(" ^ string_of_expr condition ^ ") THEN " ^ string_of_stmt_list truebody ^ string_of_elif_stmt eliflist  ^ " ELSE " ^ string_of_stmt_list elsebody ^ "\n"
 and string_of_elif_stmt = function
| [] -> ""
| (condition, body) :: rest -> " ELIF " ^ string_of_expr condition ^ " " ^ string_of_stmt_list body ^ string_of_elif_stmt rest 
and  string_of_stmt_list = function
  | [] -> ""
  | stmt :: rest -> string_of_stmt stmt ^ string_of_stmt_list rest