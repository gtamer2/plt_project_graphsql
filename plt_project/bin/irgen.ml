

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let translate stmt_list =
  let context = L.global_context () in
  let the_module = L.create_module context "GraphSQL" in 

  let i32_t = L.i32_type context in 
  let builder = L.builder context in

  (* Return the LLVM type for a MicroC type *)
  let ltype_of_typ = function
      Int   -> i32_t
  in
  
  (* define the main function & program entry point *)
  let main_type = L.function_type i32_t [||] in
  let main_func = L.define_function "main" main_type the_module in
  let entry_block = L.append_block context "entry" main_func in
  L.position_at_end entry_block builder;

  (* for now there is a single map, will need multiple maps, likely one for each function *)
  let vars_map : L.llvalue StringMap.t = StringMap.empty in

  let lookup n v_map = 
    try Some (StringMap.find n v_map)
    with Not_found -> None
  in

  let rec build_expr builder (t, e) vmap = match e with
    | SLit i -> (L.const_int i32_t i), vmap
    | SBinop (e1, op, e2) ->
      let e1', vmap1 = build_expr builder e1 vmap in
      let e2', vmap2 = build_expr builder e2 vmap1 in
      begin match op with
        | A.Add -> (L.build_add e1' e2' "addtmp" builder), vmap2
        | _ -> raise (Invalid_argument "operation not supported")
      end
    | SVar var -> (L.build_load (match (lookup var vmap) with 
          Some v -> v
        | None -> raise (Invalid_argument "variable not found")) var builder), vmap
    | SAsn (s, e) -> let e', vmap' = build_expr builder e vmap in
      let llval = (match (lookup s vmap') with
          Some v -> v
        | None -> L.build_alloca (ltype_of_typ t) s builder) in 
      let vmap'' = StringMap.add s llval vmap' in
      ignore(L.build_store e' llval builder); (e', vmap'')
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