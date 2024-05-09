


module L = Llvm
module A = Ast
open Sast


module StringMap = Map.Make(String)

let translate expr sast_env =
  let context = L.global_context () in

  (* create llvm module to generate code *)
  let the_module = L.create_module context "GraphSQL" in
  (* also need string *)
  (* and i8_ptr_t = L.pointer_type (L.i8_type context)  (* LLVM type for string *) *) 
  let i32_t = L.i32_type context
  (* and f32_t = L.float_type context
  and i1_t  = L.i1_type context  *)
  in 

  (* let builder = L.builder_at_end context (L.entry_block the_function) in *)
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

  ignore (build_expr builder expr);
  (* ignore (build_expr expression); *)

  the_module