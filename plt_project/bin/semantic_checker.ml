
(* Semantic checking for the GraphSQL compiler *)

open Ast
open Sast

module BindMap = Map.Make(String)
module VarMap = Map.Make(String)
module GraphMap = Map.Make(String)
module StringSet = Set.Make(String)

(* Define a new environment type that includes both variable and graph maps *)
type environment = {
  bindings: unified_type BindMap.t;
  vars: sexpr VarMap.t;
  graphs: graph_element list GraphMap.t;
  (* declared_vertices: StringSet.t;   *)
}

(* Function to check a single graph element *)
let check_graph_element env elem =
  (* RETURNS TUPLE, where first element is a tuple of (graph_elt_type, graph_elt), env *)
  begin match elem with
  | Vertex id ->
    (VertexType, SVertex { sid = id }), env
  | Edge (source, target, weight) ->
    (EdgeType, SEdge { ssource = source; starget = target; sweight = weight }), env
  | _ -> failwith("unsupported graph element")
  end

let ast_typ_to_sast_typ = function
  | Ast.Int -> Sast.Int
  | Ast.Bool -> Sast.Bool
  | Ast.Float -> Sast.Float
  | Ast.String -> Sast.String

let check init_env init_program = 

  (* Return a variable from our symbol table *)
  let type_of_identifier s bindings =
    try BindMap.find s bindings
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in
  
  (* return a semantically checked expression, which also constructs the environment *)
  let rec check_expr env = function
      Lit l -> ((Int, SLit l), env)
    | BoolLit l -> ((Bool, SBoolLit l), env)
    | Var var -> 
      let var_type = type_of_identifier var env.bindings in
      begin match var_type with
        | Int -> ((Int, SVar var), env)
        | Bool -> ((Bool, SVar var), env)
        | Float -> ((Float, SVar var), env)
        | String -> ((String, SVar var), env)
      end
    | FloatLit f -> ((Float, SFloatLit f), env)
    | Binop (e1, op, e2) as e ->
      let ((t1, e1'), env1) = check_expr env e1 in
      let ((t2, e2'), env2) = check_expr env1 e2 in
      let err = "illegal binary operator " ^
                string_of_typ (t1) ^ " " ^ string_of_op op ^ " " ^
                string_of_typ (t2) ^ " in " ^ string_of_expr e
      in
      (* All binary operators require operands of the same type*)
      if t1 = t2 then
        let t = match op with 
            Add | Sub | Mul | Div | Mod when t1 = Int -> Int
          | Add | Sub | Mul | Div when t1 = Float -> Float
          | Eq | Neq -> Bool
          | Gteq | Lteq | Gt | Lt when t1 = Int || t1 = Float -> Bool
          | And | Or when t1 = Bool -> Bool
          | _ -> raise (Failure err)
        in
        ((t, SBinop((t1, e1'), op, (t2, e2'))), env2)
      else if t1 = Float && t2 = Int || t1 = Int && t2 = Float then
        let t = match op with
            Add | Sub | Mul | Div -> Float
          | Eq | Neq | Gteq | Lteq | Gt | Lt -> Bool
          | _ -> raise (Failure err)
        in
        ((t, SBinop((t1, e1'), op, (t2, e2'))), env2)
      else
        raise (Failure err)
    
    | Graph (graph_elements) ->
      (* checked_graph... will be list of tuple of ((graph_elt_type, graph_elt), env) *)
      (* If one of the graph_elements is not valid object Vertex or Edge, this operation will fail *)
      (* So we want code that does the try/catch ocaml pattern *)
      (* assuming success, or upon sanity check success, return ((GraphType, SGraph elts), final_env) *)
      let checked_graph_elements_with_envs = List.map (check_graph_element env) graph_elements in

      (* Extract stuff... note we assume valid at this point because of failwith thrown in check_graph_element *)
      let checked_graph_elements = List.map fst checked_graph_elements_with_envs in
      let types = List.map fst checked_graph_elements in
      let sexprs = List.map snd checked_graph_elements in
  
      (* create the return tuple and return  *)
      ((GraphType types, SGraph checked_graph_elements), env)
    
    (* | GraphAccess(graphname, fieldname) -> 
      match type_of_identifier env graphname with
      | GraphType _ ->  
        begin match GraphMap.find_opt graphname env.graphs with
        | Some graph_elements ->
          let v_output : sgraph_element list ref = ref [] in
          let e_output : sgraph_element list ref = ref [] in
          List.iter (fun element ->
            let (checked_element, _) = check_graph_element env element in
            match snd checked_element with
            | SVertex id -> v_output := checked_element :: !v_output
            | SEdge (source, target, weight) -> e_output := checked_element :: !e_output
            | _ -> failwith ("Not a graph element")
          ) graph_elements;
          let result = match fieldname with 
            | "vertices" -> (GraphType VertexType, SGraph !v_output)  
            | "edges" -> (GraphType EdgeType, SGraph !e_output)      
            | _ -> raise (Failure ("Invalid field name: " ^ fieldname))
          in
          result, env
        | None -> raise (Failure ("Graph not found: " ^ graphname))
        end
      | _ -> raise (Failure ("Identifier '" ^ graphname ^ "' does not refer to a graph")) *)
       
    | GraphAsn(var, e) ->
      let ((t, se), env1) = check_expr env e in
      begin match t with
      | GraphType t ->
        let env2 = { env1 with bindings = BindMap.add var (GraphType t) env1.bindings } in 
        let env3 = { env2 with vars = VarMap.add var ((GraphType t), se) env2.vars } in
        (((GraphType t), SGraphAsn(var, se)), env3)
      | _ -> raise (Failure ("Graph assignment expects a graph, got " ^ string_of_typ t))
      end 
    | Asn (var, e) ->
      (* let str = var ^ " = " ^ string_of_expr e in
        Printf.printf "variable Assignment: %s\n" str; *)
      let ((t, e'), env1)  = check_expr env e in
        let env2 = { env1 with bindings = BindMap.add var t env1.bindings } in 
        let env3 = { env2 with vars = VarMap.add var (t, e') env2.vars } in
        ((t, SAsn(var, (t, e'))), env3)
    | _ -> failwith "not supported"
      in
  let rec check_stmt_list env = function
        [] -> ([], env)
      | stmt :: rest ->
        let (result_sstmt, new_env) = check_stmt env stmt in
        let (result_sl, new_env') = check_stmt_list new_env rest in
        (result_sstmt :: result_sl, new_env')
    and
    check_stmt env = function
      | Block sl ->
        let (sstmt_list', env') = check_stmt_list env sl in
        (SBlock(sstmt_list'), env')
      | Expr e -> 
        let (sexpr, env') = check_expr env e in
        (SExpr(sexpr), env')
      | _ -> failwith "not supported"
  
  in
  check_stmt_list init_env init_program
