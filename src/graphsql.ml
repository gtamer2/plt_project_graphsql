open Ast
open Printf
module VarMap = Map.Make(String)
module GraphMap = Map.Make(String)

(* Define a new environment type that includes both variable and graph maps *)
type environment = {
  vars: int VarMap.t;
  graphs: Ast.graph_element list GraphMap.t;
}

(* Initial empty environment *)
let empty_env = {
  vars = VarMap.empty;
  graphs = GraphMap.empty;
}

let rec eval env = function
  | Lit(x) -> (Lit x, env)
  | Binop(e1, op, e2) ->
    let (v1, env1) = eval env e1 in
    let (v2, env2) = eval env e2 in
    let result = match (v1, v2) with
      | (Lit v1, Lit v2) ->
          (match op with
          | Add -> Lit (v1 + v2)
          | Sub -> Lit (v1 - v2)
          | Mul -> Lit (v1 * v2)
          | Div -> Lit (v1 / v2))
      | _ -> failwith "Invalid operands for binary operation" in
    (result, env2)
  | Seq(e1, e2) ->
      let (_, env1) = eval env e1 in
      eval env1 e2
  | Var(var) ->
      (match VarMap.find_opt var env.vars with
      | Some value -> (Lit value, env)
      | None -> failwith ("Variable not found: " ^ var))
  | Graph(graph_elements) ->
      (Graph graph_elements, env)
  | Asn(var, e) ->
    let (value, env1) = eval env e in
    match value with
    | Lit x ->
      let env2 = { env1 with vars = VarMap.add var x env1.vars } in
      (Lit x, env2)
    | _ -> failwith "Assignment expects a literal integer"
  | GraphAsn(var, e) ->
    let (value, env1) = eval env e in
    match value with
    | Graph graph_elements ->
      let env2 = { env1 with graphs = GraphMap.add var graph_elements env1.graphs } in
      (Graph graph_elements, env2)
    | _ -> failwith "Graph assignment expects a graph"


let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.tokenize lexbuf in
  (* Printf.printf "Result: %s\n" (string_of_expr expr) *)
  let result, _ = eval empty_env expr in
  Printf.printf "Result: %s\n" (string_of_expr result)
