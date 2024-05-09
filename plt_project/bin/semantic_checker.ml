
(* Semantic checking for the GraphSQL compiler *)

open Ast
open Sast

module BindMap = Map.Make(String)
module VarMap = Map.Make(String)
module GraphMap = Map.Make(String)

(* Define a new environment type that includes both variable and graph maps *)
type environment = {
  bindings: unified_type BindMap.t;
  vars: sexpr VarMap.t;
  graphs: sexpr GraphMap.t;
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

let get_unique_sgraph_elements sgraph_element_list =
  List.fold_left (fun acc elem ->
    (* handle the case with duplicate edges *)
    begin match elem with
      | (EdgeType, SEdge { ssource; starget; sweight }) ->
          let existing = List.find (fun e ->
            begin  match e with
              | (EdgeType, SEdge { ssource = es; starget = et; sweight = _ }) -> es = ssource && et = starget
              | _ -> false
            end 
          ) acc in
          (* add up the weights for duplicate edges *)
          begin match existing with
            | (EdgeType, SEdge { ssource = _; starget = _; sweight = eweight }) ->
                (*  filters out the old existing edge from the accumulator list (acc) *)
                let filtered_list = List.filter (fun x -> x <> existing) acc in 
                (EdgeType, SEdge { ssource; starget; sweight = sweight + eweight }) :: filtered_list
            | _ -> elem :: acc
          end
      | _ -> if List.exists ((=) elem) acc then acc else elem :: acc
    end
  ) [] sgraph_element_list

let get_common_sgraph_elements graph1_elements graph2_elements = 
  (* filter out all common elements from graph1 *)
  let common_elements =     
    List.filter (fun elem1 ->
      List.exists (fun elem2 ->
          match elem1, elem2 with
          | (EdgeType, SEdge { ssource = s1; starget = t1; sweight = w1 }),
            (EdgeType, SEdge { ssource = s2; starget = t2; sweight = w2 }) ->
              (* do not care about the weight for now *)
              s1 = s2 && t1 = t2 
          | (VertexType, SVertex { sid = id1 }), (VertexType, SVertex { sid = id2 }) ->
              id1 = id2
          | _, _ -> false
      ) graph2_elements
  ) graph1_elements in
  
  (* when exist duplicate common edges, keep the one with lesser weight*)
  let adjusted_elements = List.map (fun elem ->
    match elem with
    | (EdgeType, SEdge { ssource; starget; sweight }) ->
        let other = List.find_opt (fun e ->
            match e with
            | (EdgeType, SEdge { ssource = os; starget = ot; sweight = _ }) -> os = ssource && ot = starget
            | _ -> false
        ) graph2_elements in
        begin match other with
        | Some (EdgeType, SEdge { ssource = _; starget = _; sweight = ow }) ->
            (EdgeType, SEdge { ssource; starget; sweight = min sweight ow })
        | _ -> elem
        end
    | _ -> elem
  ) common_elements in 
  adjusted_elements

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
        | GraphType var_type -> ((GraphType var_type, SVar var), env)
        | Int -> ((Int, SVar var), env)
        | Bool -> ((Bool, SVar var), env)
        | Float -> ((Float, SVar var), env)
        | String -> ((String, SVar var), env)
        | GraphType var_type -> ((GraphType var_type, SVar var), env)
      end
    | FloatLit f -> ((Float, SFloatLit f), env)
    | Uniop (op, e1) ->
      let ((t1, e1'), env1) = check_expr env e1 in
      let t = match op with 
          Not when t1 = Bool -> Bool
        | _ -> failwith "failed uniary op"
      in ((t, SUniop(op, (t1, e1'))), env1)
    | Binop (e1, op, e2) as e ->
      let ((t1, e1'), env1) = check_expr env e1 in
      let ((t2, e2'), env2) = check_expr env1 e2 in
      let err = "illegal binary operator " ^
                (* string_of_typ (t1) ^ " " ^ string_of_op op ^ " " ^
                string_of_typ (t2) ^ " in " ^ string_of_expr e *)
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
    
    | GraphAccess(graphname, fieldname) -> 
      let binding_type = BindMap.find graphname env.bindings in 
      (* Printf.printf "binding_type: %s\n" (string_of_typ binding_type);
      Printf.printf "graphname: %s\n" graphname; *)
      let schecked_graph_elts = GraphMap.find graphname env.graphs in
      let schecked_graph_elements_types = fst schecked_graph_elts in
      let schecked_graph_elements_exprs = snd schecked_graph_elts in
      let sgraph_elements_list = get_graph_sx schecked_graph_elements_exprs in
      (* let graph_elements_types = List.map fst schecked_graph_elements in *)
      begin match sgraph_elements_list with
      | sgraph_elem_list ->
        let v_output : sgraph_element list ref = ref [] in
        let e_output : sgraph_element list ref = ref [] in
        List.iter (fun sgraph_elem ->
          let graph_element_type = snd sgraph_elem in
          begin match graph_element_type with
            | SVertex svertex -> v_output := sgraph_elem :: !v_output
            | SEdge sedge -> e_output := sgraph_elem :: !e_output
            | _ -> failwith ("Not a graph element type")
          end 
        ) sgraph_elements_list;
        (* Get the graph_element_type list for v_output and e_output (sgraph_element list) *)
        let v_output_types = List.map (fun x -> fst x) !v_output in
        let e_output_types = List.map (fun x -> fst x) !e_output in 
        let result = begin match fieldname with 
            | "vertices" -> (GraphType v_output_types, SGraph !v_output)  
            | "edges" -> (GraphType e_output_types, SGraph !e_output)      
            | _ -> raise (Failure ("Invalid field name: " ^ fieldname))
          end
        in
        result, env
      | _ -> raise (Failure ("Graph not found: " ^ graphname))
      end

    | GraphQuery(gname1, gname2, queryType) ->
      let graph1 = GraphMap.find gname1 env.graphs in
      let graph1_element_types = fst graph1 in
      let graph1_element_sexpr = snd graph1 in
      let graph1_sgraph_element_list = get_graph_sx graph1_element_sexpr in

      let graph2 = GraphMap.find gname2 env.graphs in
      let graph2_element_types = fst graph2 in
      let graph2_element_sexpr = snd graph2 in
      let graph2_sgraph_element_list = get_graph_sx graph2_element_sexpr in


      begin match queryType with 
        | "union" ->
          (* return union result in the form of ((GraphType updated_graph_element_type_list, SGraph updated_sgraph_element_list), env) *)
          (* get everything based on sgraph_element_list *)
          let all_elements = graph1_sgraph_element_list @ graph2_sgraph_element_list in
          let unique_elements = get_unique_sgraph_elements all_elements in
          let unique_elements_types = List.map fst unique_elements in
          ((GraphType unique_elements_types, SGraph unique_elements), env)
        | "intersect" ->
          let common_elements = get_common_sgraph_elements graph1_sgraph_element_list graph2_sgraph_element_list in
          let common_elements_types = List.map fst common_elements in
          ((GraphType common_elements_types, SGraph common_elements), env)
        | _ -> failwith ("Graph query type not supported: " ^ queryType)
      end

    | GraphAsn(var, e) ->
      (* let graph_elements = get_graph_elements e in  *)
      let ((t, se), env1) = check_expr env e in
      begin match t with
      | GraphType t ->
        let env2 = { env1 with bindings = BindMap.add var (GraphType t) env1.bindings } in 
        let env3 = { env2 with graphs = GraphMap.add var (GraphType t, se) env2.graphs } in
        (((GraphType t), SGraphAsn(var, se)), env3)
      | _ -> raise (Failure ("Graph assignment expects a graph, got " ^ string_of_typ t))
      end 

    | GraphOp(gname, graph_elements, optype) ->
      let schecked_graph_elts = GraphMap.find gname env.graphs in
      let schecked_graph_elements_types = fst schecked_graph_elts in
      let schecked_graph_elements_exprs = snd schecked_graph_elts in
      (* Get the sgraph_element list and types list for existing graph elements*)
      let sgraph_elements_list = get_graph_sx schecked_graph_elements_exprs in
      let graph_element_type_list = List.map (fun x -> fst x) sgraph_elements_list in

      let checked_graph_elements_with_envs = List.map (check_graph_element env) graph_elements in
      let sgraph_element_list_input = List.map (fun x -> fst x) checked_graph_elements_with_envs in 

      (* iterate through sgraph_element list of graph_elements input
         get a list of sgraph_element for vertices and one for vertices*)
      let v_output : sgraph_element list ref = ref [] in
      let e_output : sgraph_element list ref = ref [] in
      List.iter (fun sgraph_element ->
        (* second elt of checked_element is the semantically checked expr *)
        begin match snd sgraph_element with
          | SVertex svertex -> v_output := sgraph_element :: !v_output
          | SEdge sedge -> e_output := sgraph_element :: !e_output
          | _ -> failwith ("Not a graph element")
        end 
      ) sgraph_element_list_input;
      (* Get the type list for v_output and e_output *)
      let v_output_types = List.map (fun x -> fst x) !v_output in
      let e_output_types = List.map (fun x -> fst x) !e_output in

      begin match optype with 
      | "insert" ->   
        let updated_graph_element_type_list = graph_element_type_list @ v_output_types @ e_output_types in
        let first_elem = GraphType updated_graph_element_type_list in
        let updated_sgraph_element_list = sgraph_elements_list @ !v_output @ !e_output in
        let second_elem = SGraph updated_sgraph_element_list in

        let updated_env = { env with graphs = GraphMap.add gname (first_elem, second_elem) env.graphs } in
        ((first_elem, second_elem), updated_env)

      | "delete" -> 
        let to_delete_sgraph_element_list = !v_output @ !e_output in
        let updated_sgraph_element_list = List.filter (fun elem -> not (List.mem elem to_delete_sgraph_element_list)) sgraph_elements_list in
        let second_elem = SGraph updated_sgraph_element_list in
        (* Printf.printf "second_elem: %s\n" (String.concat "" (List.map string_of_sgraph_element updated_sgraph_element_list)); *)

        let updated_element_type_list = List.map (fun x -> fst x) updated_sgraph_element_list in
        let first_elem = GraphType updated_element_type_list in

        let updated_env = { env with graphs = GraphMap.add gname (first_elem, second_elem) env.graphs } in
        ((first_elem, second_elem), updated_env)
      |  _ ->  failwith ("optype not supported in semantic checker")
      end 

    | GraphUpdate(gname, element) ->
      let schecked_graph_elts = GraphMap.find gname env.graphs in
      let schecked_graph_elements_types = fst schecked_graph_elts in
      let schecked_graph_elements_exprs = snd schecked_graph_elts in
      (* Get the sgraph_element list and types list for existing graph elements*)
      let sgraph_elements_list = get_graph_sx schecked_graph_elements_exprs in
      let graph_element_type_list = List.map (fun x -> fst x) sgraph_elements_list in

      let checked_element_with_env = check_graph_element env element in
      let sgraph_element_input = fst checked_element_with_env in 
      let element_type = fst sgraph_element_input in

      (* update sgraph_elements_list based on sgraph_element_input *)
      let updated_sgraph_element_list = 
        List.map (fun (type_info, sgraph_elem) ->  (* Assuming tuple structure (type_info, sgraph_element_x) *)
          match sgraph_elem, element with 
          | SEdge {ssource; starget; sweight}, Edge(source, target, weight)
            when ssource = source && starget = target ->
              (type_info, SEdge { ssource = source; starget = target; sweight = weight })
          | _ -> (type_info, sgraph_elem)  (* Return untouched if no update is needed *)
        ) sgraph_elements_list in

      let updated_graph_element_type_list = List.map fst updated_sgraph_element_list in
      ((GraphType updated_graph_element_type_list, SGraph updated_sgraph_element_list), env)

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
