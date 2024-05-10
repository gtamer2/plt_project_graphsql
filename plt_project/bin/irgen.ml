

module L = Llvm
module A = Ast
open Sast


module StringMap = Map.Make(String)

let translate stmt_list =
  (* print_string "test1/n"; *)
  let context = L.global_context () in
  (* print_string "test2/n"; *)
  (* create llvm module to generate code *)
  let the_module = L.create_module context "GraphSQL" in 
  (* print_string "test3\n"; *)
  let i32_t = L.i32_type context
  in 
  (* print_string "test4\n"; *)
  let builder = L.builder context in
  (* print_string "test5\n"; *)
  
  let rec build_expr builder ((_, e) : sexpr) = match e with
    | SLit i -> L.const_int i32_t i
    | SBinop (e1, op, e2) ->
      (* print_string "test6\n"; *)
      let e11 = build_expr builder e1 in
      (* print_string "test7\n"; *)
      let e22 = build_expr builder e2 in 
      (match op with
          A.Add -> 
            (* string_of_expr op; *)
            L.build_add 
          | _ -> raise (Invalid_argument "operation not supported")
      ) e11 e22 "tmp" builder
    | _ -> raise (Invalid_argument "expression type not supported")
    (* end *)
    in 

  let rec build_sstmt builder sstmt = match sstmt with
    | SExpr e -> ignore(build_expr builder e); builder
  in 

  let rec build_sstmt_list builder sstmt_list = match sstmt_list with
    | [stmt] -> ignore (build_sstmt builder stmt); builder
    | _ -> failwith "fail"
  in
  (* print_string "TEST TEST\n"; *)
  build_sstmt_list builder stmt_list;
  the_module