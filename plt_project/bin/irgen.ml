

module L = Llvm
module A = Ast
open Sast


module StringMap = Map.Make(String)

let translate stmt_list =
  let context = L.global_context () in

  (* create llvm module to generate code *)
  let the_module = L.create_module context "GraphSQL" in 
  let i32_t = L.i32_type context
  in 

  let builder = L.builder context in

  let rec build_expr builder ((_, e) : sexpr) = match e with
  SLit i -> L.const_int i32_t i
| SBinop (e1, op, e2) ->
  let e11 = build_expr builder e1 in
  let e22 = build_expr builder e2 in 
  (match op with
      A.Add -> L.build_add
      | _ -> raise (Invalid_argument "operation not supported")
  ) e11 e22 "tmp" builder
| _ -> raise (Invalid_argument "expression type not supported")
(* end *)
in 

  let rec build_sstmt builder sstmt = match sstmt with
    | SExpr e -> ignore(build_expr builder e); 
in 

let rec build_sstmt_list builder sstmt_list = match sstmt_list with
[] -> builder
| [stmt] -> ignore (build_sstmt builder stmt); builder
| _ -> failwith "fail"
in

    ignore (build_sstmt_list builder stmt_list);

  the_module