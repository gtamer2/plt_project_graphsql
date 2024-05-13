module L = Llvm
module A = Ast
open Sast
module StringMap = Map.Make(String)

let translate stmt_list =
  (* =================== IMPORTANT GLOBAL STUFF =================== *)
  let context = L.global_context () in
  let the_module = L.create_module context "GraphSQL" in 
  let gmap : L.llvalue StringMap.t = StringMap.empty in

  (* =================== PRIMITIVE TYPE TRANSLATIONS =================== *)
  let i32_t = L.i32_type context in 
  let i1_t = L.i1_type context in
  let builder = L.builder context in

  (* Return the LLVM type for a GraphSQL type *)
  let ltype_of_typ = function
      Int   -> i32_t
    | Bool -> i1_t
  in

  (* =================== GRAPH TYPE TRANSLATIONS =================== *)
  let vertex_type = L.pointer_type (L.i8_type context)
  
  in 

  (* let graph_element_type context vertex_type =
    L.struct_type context [|
      L.i32_type context;   (* Tag to indicate type: 0 for vertex, 1 for edge *)
      L.pointer_type (L.i8_type context)  (* Pointer to data so the size is dynamic *)
    |]
  
  in  *)

  let graph_type =
    L.struct_type context [|
      L.array_type vertex_type 10;  (* array of pointers to graph elements *)
      L.i32_type context; (* counter to keep track of the number of elements *)
    |]
  
  in

  (* =================== GRAPH CREATION FUNCTIONS =================== *)
  (* let create_vertex context builder vertex_id vertex_type =
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

  in  *)

  let build_empty_graph builder =
    let graph = L.build_malloc graph_type "graph" builder in
    let elements_ptr = L.build_struct_gep graph 0 "elements" builder in
    let zero = L.const_int (L.i32_type context) 0 in
  
    (* Initialize array of element pointers to null *)
    (* for i = 0 to 9 do
      let elem_ptr_ptr = L.build_gep elements_ptr [| L.const_int (L.i32_type context) i |] "elem_ptr" builder in
      ignore (L.build_store (L.const_null vertex_type) elem_ptr_ptr builder);
    done; *)
  
    (* Initialize the element count to 0 *)
    let count_ptr = L.build_struct_gep graph 1 "count" builder in
    ignore (L.build_store zero count_ptr builder);
  
    graph

  in

  (* Convert "sgraph_elements" which is a list of sgraph_elements into a list of element_ptrs *)
  (* let convert_sgraph_elements_to_ptrs context builder sgraph_elements vmap =
    let vertex_type = vertex_type context in
    let rec convert_element_to_ptr elem = match snd elem with
      | SVertex { sid = id } -> 
        let is_vertex_in_vmap = StringMap.mem id vmap in 
        if is_vertex_in_vmap then
          StringMap.find id vmap
        else begin
          let new_vertex_ptr = create_vertex context builder id vertex_type in 
          StringMap.add id new_vertex_ptr vmap; (* Add the new vertex pointer with id as the key *)
          new_vertex_ptr
        end
      | _ -> raise (Invalid_argument "Graph element has to be a Vertex")
    in
    List.map convert_element_to_ptr sgraph_elements

  in *)

  (* let add_element_to_graph context builder graph graph_element_type element_ptr =
    let elements_ptr = L.build_struct_gep graph 0 "elements" builder in
    let count_ptr = L.build_struct_gep graph 1 "count" builder in
    let count = L.build_load count_ptr "count" builder in
    let elem_ptr_ptr = L.build_gep elements_ptr [| count |] "elem_ptr" builder in
  
    ignore (L.build_store element_ptr elem_ptr_ptr builder);
    
    (* Increment the count of elements in the graph *)
    let new_count = L.build_add count (L.const_int (L.i32_type context) 1) "new_count" builder in
    ignore (L.build_store new_count count_ptr builder);
  
    graph

  in *)
  (* =================== MAIN FXN & PROGRAM ENTRY POINT =================== *)
  let main_type = L.function_type i32_t [||] in
  let main_func = L.define_function "main" main_type the_module in

  (* Create the entry block if it doesn't exist *)
  let entry_block =
    match L.entry_block main_func with
    | block -> block
    | _ -> L.append_block context "entry" main_func
  in
  L.position_at_end entry_block builder;

  (* for now there is a single map, will need multiple maps, likely one for each function *)
  let vars_map : L.llvalue StringMap.t = StringMap.empty in

  let lookup n v_map = 
    try Some (StringMap.find n v_map)
    with Not_found -> None
  in

  let rec build_expr builder (t, e) vmap = match e with
    | SLit i -> (L.const_int i32_t i), vmap
    | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0), vmap
    | SUniop (op, e1) -> let e1', vmap1 = build_expr builder e1 vmap in
      begin match op with
        | A.Not -> (L.build_not e1' "bool_tmp" builder), vmap1
        | _ -> raise (Invalid_argument "operation not supported")
      end
    | SBinop (e1, op, e2) ->
      let e1', vmap1 = build_expr builder e1 vmap in
      let e2', vmap2 = build_expr builder e2 vmap1 in
      begin match op with
        | A.Add -> (L.build_add e1' e2' "add_tmp" builder), vmap2
        | A.Sub -> (L.build_sub e1' e2' "sub_tmp" builder), vmap2
        | A.Mul -> (L.build_mul e1' e2' "mult_tmp" builder), vmap2
        | A.Div -> (L.build_sdiv e1' e2' "div_tmp" builder), vmap2
        | A.Eq -> (L.build_icmp L.Icmp.Eq e1' e2' "eq" builder), vmap2
        | A.Neq -> (L.build_icmp L.Icmp.Ne e1' e2' "eq" builder), vmap2
        | A.Gt -> (L.build_icmp L.Icmp.Sgt e1' e2' "eq" builder), vmap2
        | A.Lt -> (L.build_icmp L.Icmp.Slt e1' e2' "eq" builder), vmap2
        | A.Gteq -> (L.build_icmp L.Icmp.Sge e1' e2' "eq" builder), vmap2
        | A.Lteq -> (L.build_icmp L.Icmp.Sle e1' e2' "eq" builder), vmap2
        | A.And -> (L.build_and e1' e2' "bool_tmp" builder), vmap2
        | A.Or ->  (L.build_or e1' e2' "bool_tmp" builder),  vmap2
        | _ -> raise (Invalid_argument "operation not supported")
      end
    | SVar var ->
      let mem_location = begin match (lookup var vmap) with
        Some v -> v
        | None -> raise (Invalid_argument "variable not found")
      end in
      let load_name = ("load_tmp" ^ var) in
      (L.build_load mem_location load_name builder), vmap
    | SAsn (s, e) -> let e', vmap' = build_expr builder e vmap in
      let llval = (match (lookup s vmap') with
          Some v -> v
        | None -> L.build_alloca (ltype_of_typ t) s builder) in 
      let vmap'' = StringMap.add s llval vmap' in
      ignore(L.build_store e' llval builder); (e', vmap'')
    | SGraph sgraph_elements ->
      let graph = build_empty_graph builder in
      (* let sgraph_elem_ptrs = convert_sgraph_elements_to_ptrs context builder sgraph_elements vmap in
      let add_element_wrapper = (fun (g, vmap') elem ->
        let g' = add_element_to_graph context builder g graph_element_type elem in 
        (g',vmap')
      ) in
      let graph', _, _ = List.fold_left add_element_wrapper (graph, vmap) sgraph_elem_ptrs in
      graph', vmap  *)
      graph, vmap
    | SGraphAsn (gname, sexpr) ->
      let graph, _ = build_expr builder (Int,sexpr) vmap in (*Not sure how to add GraphType here, just using Int as a dummy*)
      let vmap' = StringMap.add gname graph vmap in
      graph, vmap'
    | _ -> raise (Invalid_argument "expression type not supported")
  in

  let rec build_sstmt (builder, vmap) sstmt = match sstmt with
    | SExpr e -> let _, vmap' = build_expr builder e vmap in builder, vmap'
    | _ -> builder, vmap
  in 

  List.fold_left build_sstmt (builder, StringMap.empty) stmt_list;
  (* List.iter (fun stmt -> ignore (build_sstmt builder stmt)) stmt_list; *)
  (* add a return to the main function *)
  let _ = L.build_ret (L.const_int i32_t 0) builder in

  the_module
