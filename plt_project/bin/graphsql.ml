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


(* let rec eval_stmt_list env = function *)
  (* | [] -> (BoolLit true, env) (* Return a default value indicating successful evaluation *)
  | stmt :: rest ->
      let (result, new_env) = eval_stmt env stmt in *)
      (* Check if there's an early termination condition *)
      (* match result with
      | BoolLit b when not b -> (BoolLit false, new_env) (* Propagate the early termination *)
      | _ -> eval_stmt_list new_env rest Continue evaluating the rest of the statements *)


let rec eval_expr env = function
  | expr -> 
    Printf.printf "Evaluating expression: %s\n" (string_of_expr expr); 
    begin match expr with
    
    | Lit(x) -> (Lit x, env)
    | FloatLit(f) -> (FloatLit f, env) 
    | BoolLit(b) -> (BoolLit b, env)  
    | Binop(e1, op, e2) ->
      let (v1, env1) = eval_expr env e1 in
      let (v2, env2) = eval_expr env1 e2 in
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
      (* Printf.printf "Graph Assignment: %s\n" str; *)
      begin match e with
        | Graph(graph_elements) ->
          let env1 = { env with graphs = GraphMap.add var graph_elements env.graphs } in
          (Graph graph_elements, env1)
        | GraphAccess(graphname, fieldname) -> 
          let (graph, env1) = eval_expr env (GraphAccess(graphname, fieldname)) in
          begin match graph with
            | Graph(graph_elements) ->
              let env2 = { env1 with graphs = GraphMap.add var graph_elements env1.graphs } in
              (Graph(graph_elements), env2)
            | _ -> failwith "GraphAccess did not return a graph"
          end
        | _ -> failwith "Graph assignment expects a graph"   
      end
    | GraphOp(gname, graph_elements, optype) ->
      begin match optype with
      | "insert" -> 
        begin match GraphMap.find_opt gname env.graphs with
        | Some existing_elements -> 
          (*Iterate through graph_elements to seperate vertices and edges*)
          let v_output : graph_element list ref = ref [] in
          let e_output : graph_element list ref = ref [] in
          List.iter (fun element ->
            match element with
            | Vertex _ -> v_output := element :: !v_output
            | Edge _ -> e_output := element :: !e_output
            | _ -> failwith ("Not a graph element")
          ) graph_elements;
        (*add vertices and edges to graph and update env.graphs GraphMap*)
        let updated_graph_elements = !v_output @ !e_output @ existing_elements in
        let updated_env = { env with graphs = GraphMap.add gname updated_graph_elements env.graphs } in
        (Graph updated_graph_elements, updated_env)
        | None -> failwith ("Graph not found: " ^ gname)
        end 
      | "delete" ->
        begin match GraphMap.find_opt gname env.graphs with
        | Some existing_elements ->
          let to_delete = List.fold_left (fun acc element -> match element with
            | Vertex vname -> (Vertex vname) :: acc
            | Edge (source, target, weight) -> (Edge (source, target, weight)) :: acc
            | _ -> acc
          ) [] graph_elements in
    
        let updated_graph_elements = List.filter (fun el -> not (List.mem el to_delete)) existing_elements in
        let updated_env = { env with graphs = GraphMap.add gname updated_graph_elements env.graphs } in
        (Graph updated_graph_elements, updated_env)
        | None -> failwith ("Graph not found: " ^ gname)
        end 
      | _ -> failwith ("Unsupported operation type: " ^ optype)
      end 
      | Asn(var, e) ->
        let str = var ^ " = " ^ string_of_expr e in
        (* Printf.printf "variable Assignment: %s\n" str; *)
        let (value, env1) = eval_expr env e in
        begin match value with
        | Lit x ->
          let env2 = { env1 with vars = VarMap.add var x env1.vars } in
          (Lit x, env2)
        | _ -> failwith "Assignment expects a literal integer" 
        end
      | _ -> failwith "not supported"
    end
 


let rec eval_stmt_list env = function 
  | [] -> (BoolLit true, env) (* Return a default value indicating successful evaluation *)
  | stmt :: rest ->
      let (result, new_env) = eval_stmt env stmt in eval_stmt_list new_env rest
    (* for each stmt eval_stmt *)
    
  and 
  eval_stmt env = function
  | stmt -> 
    begin match stmt with
    (* | Block (stmt_list) ->  *)
      (* Printf.printf "Evaluating block\n"; *)
      (* begin match stmt_list with
        | [] -> (true, env)
        | stmts -> List.fold_left (fun (_, new_env) statement -> eval_stmt new_env statement) _ env stmts
        | _ -> failwith "Invalid parsing of stmt_list"
      end *)
    | Expr (expr) -> eval_expr env expr
    | If (ifcondition, ifbody) ->
      let (v1, env1) = eval_expr env ifcondition in
      (* TODO: check that v1 is of type Bool *)
      begin match v1 with
        | (BoolLit v1) ->
          if v1 then
            let (v2, env2) = eval_stmt_list env1 ifbody in (v2, env2)
          else 
            (BoolLit v1 ,env)
        | _ -> failwith "If excepts a boolean expression" 
        end
    | IfElse (ifcondition, ifbody, elsebody)->
        let (is_true, env1) = eval_expr env ifcondition in
        (* TODO: check that v1 is of type Bool *)
        begin match is_true with
          | (BoolLit is_true) ->
            if is_true then
              let (v2, env2) = eval_stmt_list env1 ifbody in (v2, env2)
            else 
              let (v2, env2) = eval_stmt_list env1 elsebody in (v2, env2)
          | _ -> failwith "If excepts a boolean expression" 
          end
    | While (whilecondition, whilebody)->
        let (should_continue, env1) = eval_expr env whilecondition in
        begin match should_continue with
          | (BoolLit should_continue) ->
            if should_continue then
              let (_, env2) = eval_stmt_list env1 whilebody in
              eval_stmt env2 (While (whilecondition, whilebody))
            else
              (BoolLit should_continue, env1)
          | _ -> failwith "While excepts a boolean expression"
        end
    | _ -> failwith "Invalid parsing of stmt" 
    end


  let _ =
  let lexbuf = Lexing.from_channel stdin in
  let stmt_list = Parser.stmt_list Scanner.tokenize lexbuf in
  Printf.printf "Initial Expression: %s\n" (string_of_stmt_list stmt_list);
  let result, _ = eval_stmt_list empty_env stmt_list in
  Printf.printf "Result: %s\n" (string_of_expr result); 
