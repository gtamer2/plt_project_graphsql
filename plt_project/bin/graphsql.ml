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

let int_of_bool b = if b then 1 else 0

let rec eval env = function
  | expr -> 
    Printf.printf "Evaluating expression: %s\n" (string_of_expr expr); 
    begin match expr with
    | Lit(x) -> (Lit x, env)
    | FloatLit(f) -> (FloatLit f, env) 
    | BoolLit(b) -> (BoolLit b, env)  
    | Binop(e1, op, e2) ->
      let (v1, env1) = eval env e1 in
      let (v2, env2) = eval env1 e2 in
      let eval_float_op v1 op v2 =
        begin match op with 
            | Add -> FloatLit (v1 +. v2)
            | Sub -> FloatLit (v1 -. v2)
            | Mul -> FloatLit (v1 *. v2)
            | Div -> FloatLit (v1 /. v2)
            | Eq -> BoolLit (v1 = v2)
            | Neq -> BoolLit (v1 <> v2)
            | Gt -> BoolLit (v1 > v2)
            | Lt -> BoolLit (v1 < v2)
            | Gteq -> BoolLit (v1 >= v2)
            | Lteq -> BoolLit (v1 <= v2)
            | _ -> failwith "Operator is not supported for int binop int"
          end
        in
      let result = begin match (v1, v2) with
        | (Lit v1, Lit v2) ->
            begin match op with
              | Add -> Lit (v1 + v2)
              | Sub -> Lit (v1 - v2)
              | Mul -> Lit (v1 * v2)
              | Mod -> Lit (v1 mod v2)
              | Div -> Lit (v1 / v2)
              | Eq -> BoolLit (v1 = v2)
              | Neq -> BoolLit (v1 <> v2)
              | Gt -> BoolLit (v1 > v2)
              | Lt -> BoolLit (v1 < v2)
              | Gteq -> BoolLit (v1 >= v2)
              | Lteq -> BoolLit (v1 <= v2)
              | _ -> failwith "Operator is not supported for int binop int"
            end
        | (BoolLit v1, BoolLit v2) ->
            begin match op with
              | And -> BoolLit ( v1 && v2)
              | Or -> BoolLit (v1 || v2)
              | _ -> failwith ("Operator is not supported for bool op bool")
            end
        | (FloatLit v1, FloatLit v2) ->
            eval_float_op v1 op v2
        | (Lit v1, FloatLit v2) ->
            eval_float_op (float_of_int v1) op v2
        | (FloatLit v1, Lit v2) ->
            eval_float_op v1 op (float_of_int v2)
        | _ -> failwith "Invalid operands for binary operation"
        end in
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
      begin match e with
      | Graph graph_elements ->
        let env1 = { env with graphs = GraphMap.add var graph_elements env.graphs } in
        (Graph graph_elements, env1)
      | _ -> failwith "Graph assignment expects a graph"   
      end
    | Asn(var, e) ->
      let str = var ^ " = " ^ string_of_expr e in
      Printf.printf "variable Assignment: %s\n" str;
      let (value, env1) = eval env e in
      begin match value with
      | Lit x ->
        let env2 = { env1 with vars = VarMap.add var x env1.vars } in
        (Lit x, env2)
      | _ -> failwith "Assignment expects a literal integer" 
      end
    | If (ifcondition, ifbody)->
      let (v1, env1) = eval env ifcondition in
      (* TODO: check that v1 is of type Bool *)
      begin match v1 with
        | (BoolLit v1) ->
          if v1 then
            let (v2, env2) = eval env1 ifbody in (v2, env2)
          else 
            (BoolLit v1 ,env)
        | _ -> failwith "If excepts a boolean expression" 
        end
      | IfElse (ifcondition, ifbody, elsebody)->
          let (v1, env1) = eval env ifcondition in
          (* TODO: check that v1 is of type Bool *)
          begin match v1 with
            | (BoolLit v1) ->
              if v1 then
                let (v2, env2) = eval env1 ifbody in (v2, env2)
              else 
                let (v2, env2) = eval env1 elsebody in (v2, env2)
            | _ -> failwith "If excepts a boolean expression" 
            end
      | While (whilecondition, whilebody)->
          let (should_continue, env1) = eval env whilecondition in
          begin match should_continue with
            | (BoolLit should_continue) ->
              if should_continue then
                let (_, env2) = eval env1 whilebody in
                eval env2 (While (whilecondition, whilebody))
              else
                (BoolLit should_continue, env1)
            | _ -> failwith "While excepts a boolean expression"
          end
    | _ -> failwith "not supported"
    end
 

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.tokenize lexbuf in
  Printf.printf "Initial Expression: %s\n" (string_of_expr expr);
  let result, _ = eval empty_env expr in
  Printf.printf "Result: %s\n" (string_of_expr result); 
