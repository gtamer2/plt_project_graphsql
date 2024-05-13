module L = Llvm
module A = Ast
open Sast
module StringMap = Map.Make(String)
module FunctionMap = Map.Make(String)

let translate stmt_list =
  (* =================== IMPORTANT GLOBAL STUFF =================== *)
  let context = L.global_context () in
  let the_module = L.create_module context "GraphSQL" in 
  let gmap : L.llvalue StringMap.t = StringMap.empty in

  (* =================== PRIMITIVE TYPE TRANSLATIONS =================== *)
  let i32_t = L.i32_type context in 
  let i8_t = L.i8_type context in
  let i1_t = L.i1_type context in
  let builder = L.builder context in

  (* Return the LLVM type for a GraphSQL type *)
  let ltype_of_typ = function
      Int   -> i32_t
    | Bool -> i1_t
  in 

  (* =================== GRAPH TYPE TRANSLATIONS =================== *)
  let vertex_type = L.struct_type context [| L.pointer_type i8_t |] 

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
      i32_t; (* counter to keep track of the number of elements *)
    |]

  in

  (* =================== GRAPH CREATION FUNCTIONS =================== *)
  let create_vertex builder vertex_id =
    (* allocate memory for vertex *)
    let vertex_ptr = L.build_malloc vertex_type "vertex" builder in
  
    (* allocate memory for id (a string) and copy the id into it. *)
    let id_ptr = L.build_malloc i8_t "vertex_id" builder in
    ignore (L.build_store (L.build_global_stringptr vertex_id "tmp_id" builder) id_ptr builder);
  
    (* Set the vertex's ID field to point to the allocated string. *)
    let id_field_ptr = L.build_struct_gep vertex_ptr 0 "id" builder in
    ignore (L.build_store id_ptr id_field_ptr builder);
  
    vertex_ptr

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

  (* =================== FUNCTIONS CODE =================== *)
  (* map from function name to string list, each val in list is a fxn arg *)
  let function_declarations = FunctionMap.empty in
  (* map from function name to irgen code of the function body *)
  let function_bodies = FunctionMap.empty in
  
 
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

  let lookup n v_map = 
    try Some (StringMap.find n v_map)
    with Not_found -> None
  in

  (* LLVM insists each basic block end with exactly one "terminator"
    instruction that transfers control.  This function runs "instr builder"
    if the current block does not already have a terminator.  Used,
    e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
      Some _ -> ()
    | None -> ignore (instr builder) 
  
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
      let vertex_ids = List.map (fun elem -> match elem with
          | SVertex { sid = id } -> id
          | _ -> raise (Invalid_argument "Graph element has to be a Vertex")
        ) graph_element_vals in
      List.iter (fun elem -> ignore (add_vertex_to_graph builder graph elem)) vertex_ids;
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
      let zero = L.const_int (L.i32_type context) 0 in
      zero, vmap
    | _ -> raise (Invalid_argument "expression type not supported")
  in

  let rec build_sstmt (builder, vmap) sstmt = match sstmt with
    | SExpr e -> let _, vmap' = build_expr builder e vmap in builder, vmap'
    | SFunctionCreation (fname, fargs, fbody, return_type) -> 
        (* ====== BUILD FXN BODY ALGO - START ====== *)
        (* 1. Add the function declaration to fdeclr map *)
        (* 2. Generate the IR code for the body *)
        (* 3. Add the IR code to the function_bodies map *)
        (* ALGO FOR STEP 2 *)
        (* 2a. Set the builder to the start of the function
          2b. create a local variables maps, with formal args passed in
          2c. Build the code for each statement/expression in the function
          2d. Add a return if the last block falls off the end
        *)
        (* ====== BUILD FXN BODY ALGO - END ====== *)
        (* ====== Step 1 ====== *)
        ignore(StringMap.add fname fargs function_declarations);

        (* ====== Step 2 ====== *)
        (* 2a *)
        let ftype = L.function_type i32_t (Array.make (List.length fargs) i32_t) in
        let the_function = L.define_function fname ftype the_module in
        let fbuilder = L.builder_at_end context (L.entry_block the_function) in

        (* 2b *)
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
    
          
          List.fold_left2 add_formal StringMap.empty fargs
              (Array.to_list (L.params the_function))
          (* List.fold_left add_local formals fdecl.slocals *)
    
        in

        (* 2c *)

        let builder, _ = List.fold_left build_sstmt (fbuilder, local_vars) fbody in

        (* 2d *)
        ignore(add_terminal builder (L.build_ret (L.const_int i32_t 0))); builder, vmap

        (* ====== Step 3 ====== *)
        (* Don't think we need -- come back if error *)
        (* ignore(StringMap.add fname fbody function_bodies) in *)
    | _ -> builder, vmap
  in 

  List.fold_left build_sstmt (builder, StringMap.empty) stmt_list;
  let _ = L.build_ret (L.const_int i32_t 0) builder in

  the_module