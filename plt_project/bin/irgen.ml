


module L = Llvm
module A = Ast
open Sast


module StringMap = Map.Make(String)

let translate (* input *) =
  let context = L.global_context () in

  (* create llvm module to generate code *)
  let the_module = L.create_module context "GraphSQL" in
  (* also need string *)
  (* and i8_ptr_t = L.pointer_type (L.i8_type context)  (* LLVM type for string *) *) 
  let i32_t = L.i32_type context
  and f32_t = L.float_type context
  and i1_t  = L.i1_type context in 
  
  (* Return the LLVM type for a GraphSQL type *)
  let ltype_of_typ = function
      A.Lit   -> i32_t
    | A.FloatLit -> f32_t
    | A.BoolLit  -> i1_t
  in

  