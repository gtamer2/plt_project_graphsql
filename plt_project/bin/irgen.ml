module L = Llvm
module A = Ast
open Sast
module StringMap = Map.Make(String)
module FunctionMap = Map.Make(String)
 
let translate (stmt_list, functions) =
  (* =================== IMPORTANT GLOBAL STUFF =================== *)
  let context = L.global_context () in
  let the_module = L.create_module context "GraphSQL" in 

  (* =================== PRIMITIVE TYPE TRANSLATIONS =================== *)
  let i32_t = L.i32_type context in 
  let i8_t = L.i8_type context in
  let i1_t = L.i1_type context in
  let builder = L.builder context in

  (* Return the LLVM type for a GraphSQL type *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool -> i1_t
  in 

  (* =================== GRAPH TYPE TRANSLATIONS =================== *)
  let vertex_type = L.struct_type context [| L.pointer_type i8_t |] 

  in 

  let edge_type =
    L.struct_type context [|
      L.pointer_type vertex_type; 
      L.pointer_type vertex_type; 
      i32_t;
    |]

  in 

  let graph_element_type =
    L.struct_type context [|
      i1_t;   (* Tag to indicate type: 0 for vertex, 1 for edge *)
      L.pointer_type i8_t  (* Pointer to data so the size is dynamic *)
    |]

  in

  let graph_type =
    L.struct_type context [|
      L.array_type graph_element_type 10;  (* array of pointers to graph elements *)
      i32_t; (* counter to keep track of the number of elements *)
    |]

  in


  (* =================== GRAPH CREATION FUNCTIONS =================== *)
  let create_vertex builder vertex_id =
    (* 1. Build the vertex struct *)
    (* 1a. allocate memory for vertex *)
    let vertex_ptr = L.build_malloc vertex_type "vertex" builder in
  
    (* 1b. allocate memory for id (a string) and copy the id into it. *)
    let id_ptr = L.build_malloc i8_t "vertex_id" builder in
    let id = L.build_global_stringptr vertex_id "tmp_id" builder in
    ignore (L.build_store id id_ptr builder);
  
    (* 1c. Set the vertex's ID field to point to the allocated string. *)
    let id_field_ptr = L.build_struct_gep vertex_ptr 0 "id" builder in
    ignore (L.build_store id_ptr id_field_ptr builder);

    (* 2. Build Graph Element from vertex *)
    (* 2a. Allocate memory for the graph element *)
    let graph_element = L.build_malloc graph_element_type "vertex_graph_element" builder in

    (* 2b. Set the graph_element type boolean flag *)
    let zero = L.const_int i1_t 0 in
    let tag_ptr = L.build_struct_gep graph_element 0 "tag" builder in
    ignore (L.build_store zero tag_ptr builder);

    (* 2c. Set the graph_element data field to point to the vertex *)
    ignore (L.build_store vertex_ptr (L.build_struct_gep graph_element 1 "data" builder) builder);

    (* 3. Return the newly created graph element struct *)
    graph_element

  in

  let get_or_create_vertex builder graph vertex_id =
    create_vertex builder vertex_id
    (* NOTE: the commented out code below was almost complete, and would check
       for vertex conflicts in a graph before allocating more memory. Due to time constraints,
       we assume the hapy path and leave it commented out *)
(*    
    (* 1. Check if vertex exists in graph   *)
    (* 1a. Get the elements array from the graph *)
    let element_ptrs = L.build_struct_gep graph 0 "elements" builder in

    (* 1b. Get the count of elements in the graph *)
    let count_ptr = L.build_struct_gep graph 1 "count" builder in
    let count = L.build_load count_ptr "count" builder in

    (* 1c. Setup values for iteration *)
    let i = L.const_int i32_t 0 in
    let vertex_ptr = L.build_alloca vertex_type "vertex" builder in
    let found_vertex = L.build_alloca i1_t "found_vertex" builder in
    let zero = L.const_int i1_t 0 in
    let one = L.const_int i1_t 1 in

    (* 1c. Iterate through the elements array to find the vertex *)
    (* Create the Basic blocks for loo start and loop end *)
    let loop_bb = L.append_block context "loop" builder in 
    let loop_end_bb = L.append_block context "loop_end" builder in
    (* create the break statment for our loop *)
    ignore (L.build_br loop_bb builder);
  
    (* Start building the loop block *)
    L.position_at_end loop_bb builder;

    (* Code to load element and element type *)
    let ptr_to_elem = L.build_gep element_ptrs [| i |] "ptr_to_elem" builder in
    let elem = L.build_load ptr_to_elem "elem" builder in
    let tag_ptr = L.build_struct_gep elem 0 "tag" builder in
    let tag = L.build_load tag_ptr "tag" builder in *)

    in

  let create_edge builder graph_ptr src_vertex_id target_vertex_id weight =
    (* 1. Get or create src_vertex *)
    let src_vertex_ptr = get_or_create_vertex builder graph_ptr src_vertex_id in

    (* 2. Get or create target_vertex *)
    let target_vertex_ptr = get_or_create_vertex builder graph_ptr target_vertex_id in

    (* 3. Create edge *)
    (* 3a. Allocate memory for the edge *)
    let edge_ptr = L.build_malloc edge_type "edge" builder in
      
    (* 3b. Assign the vertex pointers to the edge's fields *)
    ignore (L.build_store src_vertex_ptr (L.build_struct_gep edge_ptr 0 "src_vertex" builder) builder);
    ignore (L.build_store target_vertex_ptr (L.build_struct_gep edge_ptr 1 "target_vertex" builder) builder);
  
    (* 3c. Assign the weight to the edge's field *)
    ignore (L.build_store (L.const_int i32_t weight) (L.build_struct_gep edge_ptr 2 "weight" builder) builder);

    (* 4. Create Graph element from edge *)
    (* 4a. Allocate memory for the graph element *)
    let graph_element = L.build_malloc graph_element_type "vertex_graph_element" builder in

    (* 4b. Set the graph_element type boolean flag *)
    let zero = L.const_int i1_t 1 in
    let tag_ptr = L.build_struct_gep graph_element 0 "tag" builder in
    ignore (L.build_store zero tag_ptr builder);
    
    (* 4c. Set the graph_element data field to point to the vertex *)
    ignore (L.build_store edge_ptr (L.build_struct_gep graph_element 1 "data" builder) builder);
    
    (* 5. Return *)
    graph_element

  in

  let build_empty_graph builder =
    (* Create an empty struct *)
    let graph = L.build_malloc graph_type "graph" builder in
    
    (* Initialize the element count to 0 *)
    let zero = L.const_int (L.i32_type context) 0 in
    let count_ptr = L.build_struct_gep graph 1 "count" builder in
    ignore (L.build_store zero count_ptr builder);
  
    graph

  in


  let add_vertex_to_graph builder graph vertex_id =
    (* 0. Check if the graph is full *)
    (* TODO LATER *)

    (* 1. Create a new vertex *)
    let new_vertex_ptr = create_vertex builder vertex_id in
  
    (* 2. Add ptr to new vertex to graph *)
    let elements_ptr = L.build_struct_gep graph 0 "elements" builder in
    let count_ptr = L.build_struct_gep graph 1 "count" builder in
    let count = L.build_load count_ptr "count" builder in
    let ptr_to_last_elem = L.build_gep elements_ptr [| count |] "ptr_to_last_elem" builder in
    ignore (L.build_store new_vertex_ptr ptr_to_last_elem builder);
    
    (* 3. Increment the count of elements in the graph *)
    let one = L.const_int i32_t 1 in
    let count_plus_one = L.build_add count one "count_plus_one" builder in
    ignore (L.build_store count_plus_one count_ptr builder);
  
    (* 4. On success, return pointer to graph *)
    graph

  in

  (* TODO: could probably merge with add_vertex_to_graph *)
  let add_edge_to_graph builder graph src_vertex_id target_vertex_id weight =
    (* 0. Check if the graph is full *)
    (* TODO LATER *)

    (* 1. Create a new edge *)
    let new_edge_ptr = create_edge builder graph src_vertex_id target_vertex_id weight in
  
    (* 2. Add ptr to new edge to graph *)
    let elements_ptr = L.build_struct_gep graph 0 "elements" builder in
    let count_ptr = L.build_struct_gep graph 1 "count" builder in
    let count = L.build_load count_ptr "count" builder in
    let ptr_to_last_elem = L.build_gep elements_ptr [| count |] "ptr_to_last_elem" builder in
    ignore (L.build_store new_edge_ptr ptr_to_last_elem builder);
    
    (* 3. Increment the count of elements in the graph *)
    let one = L.const_int i32_t 1 in
    let count_plus_one = L.build_add count one "count_plus_one" builder in
    ignore (L.build_store count_plus_one count_ptr builder);
  
    (* 4. On success, return pointer to graph *)
    graph

  in

  (* =================== FUNCTIONS CODE =================== *)
  (* map from function name to string list, each val in list is a fxn arg *)
  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals) in
      let return_type = ltype_of_typ fdecl.srtyp
      in let ftype = L.function_type return_type formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions 
  
  in
  
 
  (* =================== MAIN FXN & PROGRAM ENTRY POINT =================== *)
  let main_type = L.function_type i32_t [||] in
  let main_func = L.define_function "main" main_type the_module in
  let global_vars = StringMap.empty in

  (* Create the entry block if it doesn't exist *)
  let entry_block =
    match L.entry_block main_func with
    | block -> block
    | _ -> L.append_block context "entry" main_func
  in
  L.position_at_end entry_block builder;

  let lookup n v_map = 
    try Some (StringMap.find n v_map)
    with Not_found -> try 
      Some (StringMap.find n global_vars) 
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
        let graph_element_vals = List.map (fun elem -> snd elem) sgraph_elements in
        let build_graph_elt = (fun vmap builder elem -> match elem with
          | SVertex (vertex) -> add_vertex_to_graph builder graph vertex.sid
          | SEdge (edge) -> add_edge_to_graph builder graph edge.ssource edge.starget edge.sweight
          | _ -> raise (Invalid_argument "graph element type not supported")
        ) in
        List.iter (fun elem -> ignore (build_graph_elt vmap builder elem)) graph_element_vals;
        graph, vmap

        
      | SGraphAsn (gname, sexpr) ->
        let graph, e' = build_expr builder (t,sexpr) vmap in
        let mem_location = begin match (lookup gname vmap) with
          Some v -> v
          | None -> L.build_alloca graph_type gname builder
        end in
        let vmap' = StringMap.add gname graph vmap in
        ignore(L.build_store graph mem_location builder); (graph, vmap')
      | SFunctionCall ("print", [e]) ->
        let e_result, vmap = build_expr builder e vmap in
        Printf.printf "Print Function\n. Type of input expression: %s\n"(string_of_sexpr e);
        Printf.printf "\nExpression output: %s\n" (L.string_of_llvalue e_result);
        (L.const_int i32_t 0), vmap
      | SFunctionCall (function_name, args) ->
        let (fdef, fdecl) = StringMap.find function_name function_decls in
        (* let llargs = List.map (fun arg -> build_expr builder arg vmap) args in *)
        let llargs = List.map (fun arg -> fst (build_expr builder arg vmap)) args in
        let result = function_name ^ "_result" in
        (L.build_call fdef (Array.of_list llargs) result builder), vmap
      | SReturn e -> let e', vmap' = build_expr builder e vmap in
        ignore(L.build_ret e' builder); (e', vmap')
      | _ -> raise (Invalid_argument "expression type not supported")
    in
  
  let rec build_sstmt (builder, vmap) sstmt = match sstmt with
    | SExpr e -> let _, vmap' = build_expr builder e vmap in builder, vmap'
    | _ -> builder, vmap
  in
  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
      Some _ -> ()
    | None -> ignore (instr builder) 
  
  in
  let build_function_body fdecl = 
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* Construct local vars map, initialized with function's formals *)
    let local_vars =
      let add_formal m (t, n) p =
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m

      (* Allocate space for any locally declared variables and add the
      * resulting registers to our map *)
      and add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local_var m
      in

      List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function))

    in

    let builder, vmap = List.fold_left build_sstmt (builder, local_vars) fdecl.sbody in
    add_terminal builder (fun builder -> L.build_ret (L.const_int i32_t 0) builder)

  in

  (* build stmt_list and functions *)
  List.iter build_function_body functions;

  (* Build the main function *)
  let builder = L.builder_at_end context (L.entry_block main_func) in

  (* Build the main function body *)
  let builder, _ = List.fold_left build_sstmt (builder, global_vars) stmt_list in

  (* Add a return if the last block falls off the end *)
  add_terminal builder (fun builder -> L.build_ret (L.const_int i32_t 0) builder);

  the_module