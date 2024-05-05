


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

  (* for finding values bounded to variable in string map *)
  let lookup n = try StringMap.find n (* *)

  in

  let rec build_expr builder (* sast input form of expr *) = match (* part of sast input *) with
      SLit i -> L.const_int i32_t i
    | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
    | SVar s -> L.build_load (lookup s)
    | SAsn (s, e) -> build_expr builder e in 
      ignore(L.build_store e' (lookup s) builder); e'
    
    (*  | S  *)
    | SGraphAsn (s, e) -> build_expr builder e in 
    ignore(L.build_store e' (lookup s) builder); e' 

    (* type binop = Add | Sub | Mul | Div | Mod | Eq | Neq | Gteq | Lteq | Gt | Lt | And | Or *)

    | SBinop (e1, op, e2) ->
      let e1' = build_expr builder e1
      and e2' = build_expr builder e2 in 
      (match op with
          Add   -> L.build_add e1' e2' "addtmp" builder
        | Sub   -> L.build_sub e1' e2' "subtmp" builder
        | Mul   -> L.build_mul e1' e2' "multmp" builder
        | Div   -> L.build_sdiv e1' e2' "divtmp" builder (* Signed division *)
        | Mod   -> L.build_srem e1' e2' "modtmp" builder (* Signed remainder *)
        | Eq    -> L.build_icmp L.Icmp.Eq e1' e2' "eqtmp" builder
        | Neq   -> L.build_icmp L.Icmp.Ne e1' e2' "neqtmp" builder
        | Gteq  -> L.build_icmp L.Icmp.Sge e1' e2' "geqtmp" builder
        | Lteq  -> L.build_icmp L.Icmp.Sle e1' e2' "leqtmp" builder
        | Gt    -> L.build_icmp L.Icmp.Sgt e1' e2' "gttmp" builder
        | Lt    -> L.build_icmp L.Icmp.Slt e1' e2' "lttmp" builder
        | And   -> L.build_and e1' e2' "andtmp" builder
        | Or    -> L.build_or e1' e2' "ortmp" builder
      )

      (* Scall - calling functions *)

    in 

    (* let rec build_stmt builder *)

  in

  List.iter build_function_body functions;
  the_module




  let build_stmt builder = function
    (* need ast to include statements *)

  