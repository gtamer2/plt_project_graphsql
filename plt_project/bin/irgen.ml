

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let translate stmt_list =
  let context = L.global_context () in
  let the_module = L.create_module context "GraphSQL" in 

  let i32_t = L.i32_type context in 
  let builder = L.builder context in
  
  (* define the main function & program entry point *)
  let main_type = L.function_type i32_t [||] in
  let main_func = L.define_function "main" main_type the_module in
  let entry_block = L.append_block context "entry" main_func in
  L.position_at_end entry_block builder;

  let rec build_expr builder (_, e) = match e with
    | SLit i -> L.const_int i32_t i
    | SBinop (e1, op, e2) ->
      let e1' = build_expr builder e1 in
      let e2' = build_expr builder e2 in
      begin match op with
        | A.Add -> L.build_add e1' e2' "addtmp" builder
        | _ -> raise (Invalid_argument "operation not supported")
      end
    | _ -> raise (Invalid_argument "expression type not supported")
  in

  let rec build_sstmt builder sstmt = match sstmt with
    | SExpr e -> ignore(build_expr builder e); builder
    | _ -> builder
  in 

  List.iter (fun stmt -> ignore (build_sstmt builder stmt)) stmt_list;
  (* add a return to the main function *)
  let _ = L.build_ret (L.const_int i32_t 0) builder in

  the_module