module L = Llvm
module A = Ast
module S = Sast
open Sast

module StringMap = Map.Make(String)

(* Helper function to check if a statement is an expression *)
(* let is_expr = function
  | SExpr _ -> true
  | _ -> false *)

let vertex_type context = 
  (* pointer to an array of char is used to handle strings *)
  L.struct_type context [| L.pointer_type (L.i8_type context) |] 

let edge_type context vertex_type =
  L.struct_type context [|
    L.pointer_type vertex_type; 
    L.pointer_type vertex_type; 
    L.i32_type context          
  |]

let graph_element_type context vertex_type edge_type =
  L.struct_type context [|
    L.i32_type context;   (* Tag to indicate type: 0 for vertex, 1 for edge *)
    L.pointer_type (L.i8_type context)  (* Pointer to data so the size is dynamic *)
  |]

let graph_type context graph_element_type =
  L.struct_type context [|
    L.array_type (L.pointer_type graph_element_type) 10;  (* array of pointers to graph elements *)
    L.i32_type context; (* counter to keep track of the number of elements *)
  |]

let create_vertex context builder vertex_id vertex_type =
  (* allocate memory for vertex *)
  let vertex_ptr = L.build_malloc vertex_type "vertex" builder in

  (* allocate memory for id (a string) and copy the id into it. *)
  let id_len = String.length vertex_id + 1 in (* +1 for null terminator *)
  let id_ptr = L.build_array_malloc (L.i8_type context) (L.const_int (L.i32_type context) id_len) "vertex_id" builder in
  ignore (L.build_store (L.build_global_stringptr vertex_id "tmp_id" builder) id_ptr builder);

  (* Set the vertex's ID field to point to the allocated string. *)
  let id_field_ptr = L.build_struct_gep vertex_ptr 0 "id" builder in
  ignore (L.build_store id_ptr id_field_ptr builder);

  vertex_ptr

let create_edge_with_data context builder source_vertex target_vertex weight edge_type =
  (* Allocate memory for the edge *)
  let edge_ptr = L.build_malloc edge_type "edge" builder in

  let source_ptr = L.build_struct_gep edge_ptr 0 "source" builder in
  ignore (L.build_store source_vertex source_ptr builder);

  let target_ptr = L.build_struct_gep edge_ptr 1 "target" builder in
  ignore (L.build_store target_vertex target_ptr builder);

  let weight_ptr = L.build_struct_gep edge_ptr 2 "weight" builder in
  let weight_val = L.const_int (L.i32_type context) weight in
  ignore (L.build_store weight_val weight_ptr builder);

  edge_ptr

let create_edge context builder source_vertex target_vertex weight edge_type =
  (* Assume create_edge_with_data returns an initialized edge *)
  let edge_ptr = create_edge_with_data context builder source_vertex target_vertex weight edge_type in 
  (* ignore (L.build_store edge_type edge_ptr builder); *)
  edge_ptr
  
let create_graph_element context builder graph_element_t element tag =
  let ge = L.build_malloc graph_element_t "graph_element" builder in
  let tag_ptr = L.build_struct_gep ge 0 "tag" builder in
  ignore (L.build_store (L.const_int (L.i32_type context) tag) tag_ptr builder);
  
  let payload_ptr = L.build_struct_gep ge 1 "payload" builder in
  ignore (L.build_store element payload_ptr builder);
  ge


 (* Convert "sgraph_elements" which is a list of sgraph_elements into a list of element_ptrs *)
let convert_sgraph_elements_to_ptrs context builder sgraph_elements vmap =
  let vertex_type = vertex_type context in
  let edge_type = edge_type context vertex_type in
  let rec convert_element_to_ptr elem = match snd elem with
    | SVertex { sid = id } -> 
      let is_vertex_in_vmap = StringMap.mem id vmap in 
      begin match is_vertex_in_vmap with 
      | false -> 
        let new_vertex_ptr = create_vertex context builder id vertex_type in 
        StringMap.add id new_vertex_ptr vmap; (* Add the new vertex pointer with id as the key *)
        new_vertex_ptr
      | true ->
        StringMap.find id vmap
      end
      
    | SEdge { ssource = source; starget = target; sweight = weight } ->
      begin
        match StringMap.find_opt source vmap, StringMap.find_opt target vmap with
        | Some source_ptr, Some target_ptr ->
          create_edge context builder source_ptr target_ptr weight edge_type
        | _ ->
          raise (Invalid_argument "Source or target vertex not found in the map")
      end
    | _ -> raise (Invalid_argument "Graph element has to be a Vertex or an Edge")
  in
  List.map convert_element_to_ptr sgraph_elements


let build_initial_graph context builder graph_type graph_element_type =
  let graph = L.build_malloc graph_type "graph" builder in
  let elements_ptr = L.build_struct_gep graph 0 "elements" builder in
  let zero = L.const_int (L.i32_type context) 0 in

  (* Initialize array of element pointers to null *)
  for i = 0 to 9 do
    let elem_ptr_ptr = L.build_gep elements_ptr [| L.const_int (L.i32_type context) i |] "elem_ptr" builder in
    ignore (L.build_store (L.const_null (L.pointer_type graph_element_type)) elem_ptr_ptr builder);
  done;

  (* Initialize the element count to 0 *)
  let count_ptr = L.build_struct_gep graph 1 "count" builder in
  ignore (L.build_store zero count_ptr builder);

  graph

let add_element_to_graph context builder graph graph_element_type element_ptr =
  let elements_ptr = L.build_struct_gep graph 0 "elements" builder in
  let count_ptr = L.build_struct_gep graph 1 "count" builder in
  let count = L.build_load count_ptr "count" builder in
  let elem_ptr_ptr = L.build_gep elements_ptr [| count |] "elem_ptr" builder in

  ignore (L.build_store element_ptr elem_ptr_ptr builder);
  
  (* Increment the count of elements in the graph *)
  let new_count = L.build_add count (L.const_int (L.i32_type context) 1) "new_count" builder in
  ignore (L.build_store new_count count_ptr builder);

  graph

let translate stmt_list =
  let context = L.global_context () in
  let the_module = L.create_module context "GraphSQL" in 

  (* get types from context *)
  let i32_t = L.i32_type context 
  and float_t = L.double_type context
  and i8_t = L.i8_type context
  and i1_t = L.i1_type context
  and i64_t = L.i64_type context
  and void_t = L.void_type context
  in

  (* Return the LLVM type for a Graphsql type (unified_type) *)
  let ltype_of_typ = function
    A.Int   -> i32_t
    | A.Float -> float_t
    | A.String -> L.pointer_type i8_t
    | A.Bool -> i1_t
    (* need to update graph_element_type/graph_type so that they take in an argument*)
    | A.GraphElement t -> L.pointer_type (graph_element_type context vertex_type edge_type)
    | A.Graph element_list -> L.pointer_type (graph_type context (graph_element_type context vertex_type edge_type))
  in

  let sast_unified_to_ast_unified = function
    S.Int   ->  A.Int
    | S.Float -> A.Float 
    | S.String -> A.String
    | S.Bool -> A.Bool 
    (* | S.GraphElement t ->
    | S.Graph element_list *)
    in

  (* define the main function & program entry point *)
  let main_type = L.function_type i32_t [||] in
  let main_func = L.define_function "main" main_type the_module in

  let builder = L.builder context in
  (* Create the entry block if it doesn't exist *)
  let entry_block =
    match L.entry_block main_func with
    | block -> block
    | _ -> L.append_block context "entry" main_func
  in
  L.position_at_end entry_block builder;

  (* for now there is a single map, will need multiple maps, likely one for each function *)
  let vmap : L.llvalue StringMap.t = StringMap.empty in
  (* graph map *)
  let gmap : L.llvalue StringMap.t = StringMap.empty in
  (* function map *)
  
  (* we might need to declare expr? and then build_expr is used to fill in the expr body*)

  (* need to look into v_map and then graph_map and then function_map, similar to how Var() is defined in graphsql.ml*)
  (* let lookup n vmap gmap = 
    try StringMap.find n vmap
    with Not_found -> 
      try StringMap.find n gmap
      with Not_found -> None
  in *)

  let lookup n vmap gmap = 
    match StringMap.find_opt n vmap with
    | Some v -> v
    | None ->
      (match StringMap.find_opt n gmap with
      | Some v -> v
      | None -> raise (Invalid_argument "variable not found"))
  in

  (* takes in sexpr (unified_type, sx) and generate llvm code for expr*)
  let rec build_expr builder (t, e) vmap gmap = match e with
    | SLit i -> (L.const_int i32_t i), vmap, gmap
    
    (* | SFloatLit of float
    | SBoolLit of bool *)
    
    | SBinop (e1, op, e2) ->
      let e1', vmap1, gmap1 = build_expr builder (e1) vmap gmap in
      let e2', vmap2, gmap2 = build_expr builder (e2) vmap1 gmap1 in
      begin match op with
        | A.Add -> (L.build_add e1' e2' "add_tmp" builder), vmap2, gmap2
        | A.Sub -> (L.build_sub e1' e2' "sub_tmp" builder), vmap2, gmap2
        | A.Mul -> (L.build_mul e1' e2' "mult_tmp" builder), vmap2, gmap2
        | A.Div -> (L.build_sdiv e1' e2' "div_tmp" builder), vmap2, gmap2
        | A.Eq -> (L.build_icmp L.Icmp.Eq e1' e2' "eq" builder), vmap2, gmap2
        | A.Neq -> (L.build_icmp L.Icmp.Ne e1' e2' "eq" builder), vmap2, gmap2
        | A.Gt -> (L.build_icmp L.Icmp.Sgt e1' e2' "eq" builder), vmap2, gmap2
        | A.Lt -> (L.build_icmp L.Icmp.Slt e1' e2' "eq" builder), vmap2, gmap2
        | A.Gteq -> (L.build_icmp L.Icmp.Sge e1' e2' "eq" builder), vmap2, gmap2
        | A.Lteq -> (L.build_icmp L.Icmp.Sle e1' e2' "eq" builder), vmap2, gmap2
        | _ -> raise (Invalid_argument "operation not supported")
      end
    | SVar var -> (L.build_load (match (lookup var vmap gmap) with 
          v -> v
        | _ -> raise (Invalid_argument "variable not found")) var builder), vmap, gmap
    | SAsn (s, e) -> let e', vmap', gmap' = build_expr builder e vmap gmap in
      let llval = (match (lookup s vmap' gmap') with
          v -> v
        | _ -> e'
      ) in
      let vmap'' = StringMap.add s llval vmap' in
      ignore(L.build_store e' llval builder); (e', vmap'', gmap)
    (* | SUniop of uniop * sexpr *)
    
    (* | SGraph sgraph_elements ->
      let graph = build_initial_graph builder in
      List.fold_left (fun g elem -> add_element_to_graph context builder g elem) graph graph_elements, vmap, gmap *)


    (* add_element_to_graph context builder graph graph_element_type element_ptr *)
    (* update gmap *)
    (* return graph_ptr, vmap', gmap' *)

    | SGraph sgraph_elements ->
      let graph = build_initial_graph context builder (graph_type context (graph_element_type context vertex_type edge_type)) (graph_element_type context vertex_type edge_type) in
      
      (* convert sgraph_elements into element_ptrs, then use list.fold_left *)
      let sgraph_elem_ptr = convert_sgraph_elements_to_ptrs context builder sgraph_elements vmap in

      let graph', _, _ = List.fold_left (fun (g, vmap', gmap') elem ->
        
        let g' = add_element_to_graph context builder g graph_element_type elem in 
        (g',vmap',gmap')

      ) (graph, vmap, gmap) sgraph_elem_ptr in
      graph', vmap, gmap

      (* Code to check if gname is in gmap -- Ignore *)
      (* let is_graph_in_gmap = Stringmap.mem gname gmap in
      begin match is_graph_in_gmap with
        | false ->
        | true -> 
      end *)

    (* TODO - expr can give a non graph output, do not add to gmap in that case. Low Priority*)
    | SGraphAsn (gname, sexpr) ->
      let graph', vmap', gmap' = build_expr builder (Int,sexpr) vmap gmap in (*Not sure how to add GraphType here, just using Int as a dummy*)
      (* Syntax would be GraphType(sgraph_element list) with a relevant "sgraph_element list" *)
      let gmap'' = StringMap.add gname graph' gmap' in
      graph', vmap', gmap''

    | _ -> raise (Invalid_argument "expression type not supported")
  in

  let rec build_sstmt (builder, vmap, gmap) sstmt = match sstmt with
    | SExpr e -> 
      let _, vmap', gmap' = build_expr builder e vmap gmap in builder, vmap', gmap'
    | _ -> builder, vmap, gmap
  in 

  let builder = L.builder context in

  List.fold_left build_sstmt (builder, StringMap.empty, StringMap.empty) stmt_list;
  (* List.iter (fun stmt -> ignore (build_sstmt builder stmt)) stmt_list; *)
  (* add a return to the main function *)
  let _ = L.build_ret (L.const_int i32_t 0) builder in

  the_module
