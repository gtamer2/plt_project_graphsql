open Ast
open Printf
module VarMap = Map.Make(String)
module GraphMap = Map.Make(String)

(* Define a new environment type that includes both variable and graph maps *)
type environment = {
  vars: int VarMap.t;
  graphs: (string * Ast.graph_element list) list GraphMap.t;
}

(* Initial empty environment *)
let empty_env = {
  vars = VarMap.empty;
  graphs = GraphMap.empty;
}

let rec eval env = function
  | Lit(x) -> (x, env)
  | Binop(e1, op, e2) ->
    let (v1, env1) = eval env e1 in
    let (v2, env2) = eval env1 e2 in
    let result = match op with
      | Add -> v1 + v2
      | Sub -> v1 - v2
      | Mul -> v1 * v2
      | Div -> v1 / v2 in
    (result, env2)
  | Seq(e1, e2) ->
      let (_, env1) = eval env e1 in
      eval env1 e2
  | Asn(var, e) ->
    let (value, env1) = eval env e in
    let vars = VarMap.add var value env1.vars in  (* Update vars within the environment *)
    (value, { env1 with vars })  (* Return updated environment *)
  | Var(var) ->
    let value = 
      try VarMap.find var env.vars 
      with Not_found -> failwith (Printf.sprintf "Variable '%s' not found" var) in
    (value, env)
  | Graph(vertices, edges) ->
    let graph_repr = [("Vertices", vertices); ("Edges", edges)] in
    let graphs = GraphMap.add "generic_graph_key" graph_repr env.graphs in
    (0, { env with graphs })
  | _ -> failwith "Expression type not supported"


let _ =
  let lexbuf = Lexing.from_channel stdin in
  (* let expr = Parser.expr Scanner.tokenize lexbuf in *)
  let expr = Parser.program Scanner.tokenize lexbuf in
  let result, _ = eval empty_env expr in
  match expr with
  | Graph(_, _) -> 
      Printf.printf "Graph initialized: %s\n" (string_of_expr expr)
  | _ -> 
      Printf.printf "Result: %s\n" (string_of_expr expr)

  (* print_endline (string_of_int result) *)
  (* match expr with
  | Graph(_, _, _) -> print_endline "Graph created."  (* Specific handling for graph creation *)
  | _ -> print_endline (string_of_int result) *)
