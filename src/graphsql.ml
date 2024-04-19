open Ast
open Printf
module VarMap = Map.Make(String)
module GraphMap = Map.Make(String)

(* Initial empty environment *)
let empty_env = {
  vars = VarMap.empty;
  graphs = GraphMap.empty;
}

(* Define a new environment type that includes both variable and graph maps *)
type environment = {
  vars: int VarMap.t;
  (* graphs is a map from strings (graph variable names) to list of graph elements (nodes and edges mixed in)  *)
  (* note that we could store a graph as two separate lists nodes and edges, but simpler to have one list for now*)
  (* TODO: NOTE THAT THIS IS ONE SOURCE OF ERROR *)
  (* graphs: (string * Ast.graph_element list) list GraphMap.t; *)
  graphs: Ast.graph_element list GraphMap.t;
}

(* let rec eval env = function
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
  (*| Graph(vertices, edges) ->
    let graph_repr = [("Vertices", vertices); ("Edges", edges)] in
    let graphs = GraphMap.add "generic_graph_key" graph_repr env.graphs in
    (0, { env with graphs })*)
  | _ -> failwith "Expression type not supported" *)

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
      let env2 = VarMap.add var value env1 in
      (value, env2)
  | Var(var) ->
      VarMap.find var env, env  

  (* NOTE: this is to _create_ a graph *)
  (* ISSUE IDENTIFIED:  *)
  | Graph(vertices, edges) ->
    (* let graph_repr = [("Vertices", vertices); ("Edges", edges)] in *)
    let graph_repr = vertices @ edges (* concatenate vertices and edges into one list *)
    let graphs = GraphMap.add "name" graph_repr env.graphs in
    (0, { env with graphs })


let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.tokenize lexbuf in
  (* let expr = Parser.program Scanner.tokenize lexbuf in *)
  let result, _ = eval empty_env expr in
  Printf.printf "Result: %s\n" (string_of_expr expr)

(* let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.tokenize lexbuf in
  let result, _ = eval VarMap.empty expr in
  print_endline (string_of_int result) *)
  (*
  match expr with
  | Graph(_, _) -> 
      Printf.printf "Graph initialized: %s\n" (string_of_expr expr)
  | _ -> 
      Printf.printf "Result: %s\n" (string_of_expr expr)*)

  (* print_endline (string_of_int result) *)
  (* match expr with
  | Graph(_, _, _) -> print_endline "Graph created."  (* Specific handling for graph creation *)
  | _ -> print_endline (string_of_int result) *)