
(* Semantic checking for the GraphSQL compiler *)

open Ast
open Sast

module BindMap = Map.Make(String)
module VarMap = Map.Make(String)
module GraphMap = Map.Make(String)
module FunctionMap = Map.Make(String)

(* Define a new environment type that includes both variable and graph maps *)
type environment = {
  bindings: unified_type BindMap.t;
  vars: sexpr VarMap.t;
  graphs: sexpr GraphMap.t;
}

(* Function to check a single graph element *)
(* RETURNS TUPLE, where first element is a tuple of (graph_elt_type, graph_elt), env *)
let check_graph_element env elem =
  begin match elem with
  | Vertex id ->
    (VertexType, SVertex { sid = id }), env
  | Edge (source, target, weight) ->
    (EdgeType, SEdge { ssource = source; starget = target; sweight = weight }), env
  | _ -> failwith("unsupported graph element")
  end

(* Helper function to Union opertaion:
   Get a list of unique sgraph elements from a list of sgraph elements *)
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


(* Helper function to Intersect opertaion:
   Get a list of overlapping sgraph elements from two lists of sgraph elements *)
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
  
  (* For Intersect operation:
     when exist duplicate common edges, keep the one with lesser weight*)
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

(* Function used to semantically checked a list of statments and functions*)
let check (statements, functions) = 

  let init_env = {
    bindings = BindMap.empty;
    vars = VarMap.empty;
    graphs = GraphMap.empty;
  } in
  let check_assign lvaluet rvaluet err =
    if lvaluet = rvaluet then lvaluet else raise (Failure err)
  in

  (* Return a variable from our symbol table *)
  let type_of_identifier s bindings =
    try BindMap.find s bindings
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in


  let func_map = FunctionMap.empty in

  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
      _ when FunctionMap.mem n func_map -> make_err built_in_err
    | _ when FunctionMap.mem n map -> make_err dup_err
    | _ ->  FunctionMap.add n fd map
  in

  let function_decls = List.fold_left add_func func_map functions 

  in
  
  (* Return a function from our symbol table *)
  let find_func s =
    try FunctionMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  
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
      let checked_graph_elements_with_envs = List.map (check_graph_element env) graph_elements in

      (* Extract stuff... note we assume valid at this point because of failwith thrown in check_graph_element *)
      let checked_graph_elements = List.map fst checked_graph_elements_with_envs in
      let types = List.map fst checked_graph_elements in
      let sexprs = List.map snd checked_graph_elements in
  
      (* create the return tuple and return  *)
      ((GraphType types, SGraph checked_graph_elements), env)
    
    | GraphAccess(graphname, fieldname) -> 
      let binding_type = BindMap.find graphname env.bindings in 
      let schecked_graph_elts = GraphMap.find graphname env.graphs in
      let schecked_graph_elements_types = fst schecked_graph_elts in
      let schecked_graph_elements_exprs = snd schecked_graph_elts in
      let sgraph_elements_list = get_graph_sx schecked_graph_elements_exprs in
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
      let ((t, e'), env1)  = check_expr env e in
        let env2 = { env1 with bindings = BindMap.add var t env1.bindings } in 
        let env3 = { env2 with vars = VarMap.add var (t, e') env2.vars } in
        ((t, SAsn(var, (t, e'))), env3)
    | FunctionCall(fname, args) as call ->
      let fd = find_func fname in
      let param_length = List.length fd.formals in
      if List.length args != param_length then
        raise (Failure ("expecting " ^ string_of_int param_length ^
                        " arguments in " ^ string_of_expr call))
      else 
        let args_types = List.map (fun x -> fst (check_expr env x)) args in
      let any_mismatch = List.exists (fun (t1, _) -> List.exists (fun (t2, _) -> t1 != t2) args_types) fd.formals in
      if any_mismatch then
        raise (Failure ("illegal argument found in " ^ string_of_expr call))
      else
        (fd.rtyp, SFunctionCall(fname, args_types)), env
    | Return e ->
      let ((t, e'), env1) = check_expr env e in
      (t, SReturn (t, e')), env1
    | _ -> failwith "expression not supported"
      in

  (* Function used to semantically check stmt list *)
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

      (* SIf *)

      | If (cond, then_stmt) ->
        let ((t1, cond'), env1) = check_expr env cond in
        let (then_stmt', env2) = check_stmt_list env1 then_stmt in
        let err = "illegal if statement " ^
                  string_of_typ t1 ^ " " ^ "in " ^ string_of_expr cond
                  ^ " " ^ "in " ^ string_of_stmt_list then_stmt
        in
        if t1 = Bool then
          (SIf((t1, cond'), then_stmt'), env2)
        else
          raise (Failure err)

      | For (init, condition, update, body) ->
        let ((t1,init'),env1) = check_expr env init in
        let ((t2,condition'),env2) = check_expr env1 condition in 
        let ((t3,update'),env3) = check_expr env2 update in
        let (body',env4) = check_stmt_list env3 body in
        let err = "illegal for loop " ^
                  string_of_typ t1 ^ " " ^ "in " ^ string_of_expr init ^
                  string_of_typ t2 ^ " " ^ "in " ^ string_of_expr condition ^
                  string_of_typ t3 ^ " " ^ "in " ^ string_of_expr update ^
                   " " ^ "in " ^ string_of_stmt_list body
        in
        if t2 = Bool then
          (SFor((t1,init'), (t2,condition'), (t3,update'), body'), env4)
        else
          raise (Failure err)
      | IfElse (cond, then_stmt, else_stmt) ->
        let ((t1, cond'), env1) = check_expr env cond in
        let (then_stmt', env2) = check_stmt_list env1 then_stmt in
        let (else_stmt', env3) = check_stmt_list env2 else_stmt in
        let err = "illegal if-else statement " ^
                  string_of_typ t1 ^ " " ^ "in " ^ string_of_expr cond ^
                   " " ^ "in " ^ string_of_stmt_list then_stmt ^
                   " " ^ "in " ^ string_of_stmt_list else_stmt
        in
        if t1 = Bool then
          (SIfElse( (t1, cond'), then_stmt', else_stmt' ), env3)
        else
          raise (Failure err)

      | While (cond, body) ->
        let ((t1, cond'), env1) = check_expr env cond in
        let (body', env2) = check_stmt_list env1 body in
        let err = "illegal while loop " ^
                  string_of_typ t1 ^ " " ^ "in " ^ string_of_expr cond ^
                   " " ^ "in " ^ string_of_stmt_list body
        in
        if t1 = Bool then
          (SWhile((t1, cond'), body'), env2)
        else
          raise (Failure err)
      | _ -> failwith "Statement not supported"
  
  in
    (* Verify a list of bindings has no duplicate names *)
    let check_binds (kind : string) (binds : (unified_type * string) list) =
      let rec dups = function
          [] -> ()
        |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
          raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
        | _ :: t -> dups t
      in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
    in
  let check_func func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;
    { srtyp = func.rtyp;
      sfname = func.fname;
      sformals = func.formals;
      sbody = fst (check_stmt_list init_env func.body)
    }
  in
  let checked_prog_statements = fst (check_stmt_list init_env statements) in
  let checked_functions = List.map check_func functions in
  (checked_prog_statements, checked_functions)
