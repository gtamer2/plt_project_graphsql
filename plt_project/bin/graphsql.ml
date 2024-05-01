open Ast
open Printf
module VarMap = Map.Make(String)
module GraphMap = Map.Make(String)

(* Define a new environment type that includes both variable and graph maps *)
type environment = {
  vars: int VarMap.t;
  graphs: graph_element list GraphMap.t;
}

(* Initial empty environment *)
let empty_env = {
  vars = VarMap.empty;
  graphs = GraphMap.empty;
}

let rec eval env = function
  | expr -> 
    Printf.printf "Evaluating expression: %s\n" (string_of_expr expr); 
    match expr with
    | Lit(x) -> (Lit x, env)
    | FloatLit(f) -> (FloatLit f, env) 
    | BoolLit(b) -> (BoolLit b, env)    
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
        | None -> 
          match GraphMap.find_opt var env.graphs with
          | Some value -> (Graph value, env)
          | None -> failwith ("Variable not found: " ^ var))
    | Graph (graph_elements) ->
      (Graph(graph_elements), env)
    
  
    | GraphAccess(graphname, fieldname) -> 
      Printf.printf "printing expression: %s\n" (string_of_expr expr); 
      Printf.printf "printing graph: %s\n" (graphname); 
      Printf.printf "printing field: %s\n" (fieldname); 
      (* begin match GraphMap.find_opt graphname env.graphs with
        | Some graph_elements -> (Lit 0, env)
        | None -> (Lit 1, env)
      end
      (Lit 0, env) *)
      begin match GraphMap.find_opt graphname env.graphs with
        | Some graph_elements ->
            let v_output : graph_element list ref = ref [] in
            let e_output : graph_element list ref = ref [] in
            
            List.iter (fun element ->
              match element with
              | Vertex _ -> v_output := element :: !v_output
              | Edge _ -> e_output := element :: !e_output
              | _ -> failwith ("Not a graph element")
            ) graph_elements;
            match fieldname with 
              | "vertices" -> (Graph !v_output, env)
              | "edges" -> (Graph !e_output, env)
              | _ -> failwith ("Invalid field name: " ^ fieldname)
        | _ -> failwith ("Graph not found: " ^ graphname)
      end

    | GraphAsn(var, e) ->
      let str = "GraphAsn " ^ var ^ " = " ^ string_of_expr e in
      Printf.printf "Graph Assignment: %s\n" str;
      match e with
      | Graph graph_elements ->
        let env1 = { env with graphs = GraphMap.add var graph_elements env.graphs } in
        (Graph graph_elements, env1)
      | _ -> failwith "Graph assignment expects a graph"
    | Asn(var, e) ->
      let str = var ^ " = " ^ string_of_expr e in
      Printf.printf "variable Assignment: %s\n" str;
      let (value, env1) = eval env e in
      match value with
      | Lit x ->
        let env2 = { env1 with vars = VarMap.add var x env1.vars } in
        (Lit x, env2)
      | _ -> failwith "Assignment expects a literal integer"     
    
    | _ -> failwith "not supported"


 

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.tokenize lexbuf in
  Printf.printf "Initial Expression: %s\n" (string_of_expr expr);
  let result, _ = eval empty_env expr in
  Printf.printf "Result: %s\n" (string_of_expr result);
