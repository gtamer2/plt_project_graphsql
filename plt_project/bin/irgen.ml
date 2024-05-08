


module L = Llvm
module A = Ast
open Sast


module StringMap = Map.Make(String)

let translate sstmt_list sast_env =
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

  let 

  (* for finding values bounded to variable in string map *)
  let lookup n env = try StringMap.find n (* *)

  in

  let rec build_expr builder ((t, e) : sexpr) env = match e with
      SLit i -> L.const_int i32_t i
    | SFloatLit f -> L.const_float f32_t f
    | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
    | SVar s -> L.build_load (lookup s env) s builder
    | SAsn (s, e) -> build_expr builder e in 
      ignore(L.build_store e' (lookup s) builder); e'
    | SUniop (op, e1) -> 
      let e1' = build_expr builder e1 env in
        match op with 
          A.Not -> L.build_not e1 "nottmp" builder
    | SBinop (e1, op, e2) ->
      let e1' = build_expr builder e1
      and e2' = build_expr builder e2 in 
      match t, op with
          Int, Add   -> L.build_add e1' e2' "addtmp" builder
        | Int, Sub   -> L.build_sub e1' e2' "subtmp" builder
        | Int, Mul   -> L.build_mul e1' e2' "multmp" builder
        | Int, Div   -> L.build_sdiv e1' e2' "divtmp" builder (* Signed division *)
        | Int, Mod   -> L.build_srem e1' e2' "modtmp" builder (* Signed remainder *)
        | Float, Add   -> L.build_fadd e1' e2' "addtmp" builder
        | Float, Sub   -> L.build_fsub e1' e2' "subtmp" builder
        | Float, Mul   -> L.build_fmul e1' e2' "multmp" builder
        | Float, Div   -> L.build_fdiv e1' e2' "divtmp" builder
        | _, Eq    -> L.build_icmp L.Icmp.Eq e1' e2' "eqtmp" builder
        | _, Neq   -> L.build_icmp L.Icmp.Ne e1' e2' "neqtmp" builder
        | _, Gteq  -> L.build_icmp L.Icmp.Sge e1' e2' "geqtmp" builder
        | _, Lteq  -> L.build_icmp L.Icmp.Sle e1' e2' "leqtmp" builder
        | _, Gt    -> L.build_icmp L.Icmp.Sgt e1' e2' "gttmp" builder
        | _, Lt    -> L.build_icmp L.Icmp.Slt e1' e2' "lttmp" builder
        | _, And   -> L.build_and e1' e2' "andtmp" builder
        | _, Or    -> L.build_or e1' e2' "ortmp" builder
      

      (* Scall - calling functions *)

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

    (* let rec build_stmt builder *)
    (* builder acts different from other things *)

    let rec build_sstmt_list builder sstmt_list env = match sstmt_list with
        [] -> builder, env
      | sstmt :: rest_list ->
        let (builder', env') = build_sstmt builder sstmt env in
        let (builder'', env'') = build_sstmt_list builder' rest_list env' in
        builder'', env''
    in

    let rec build_sstmt builder sstmt env = match sstmt with
        SBlock sl -> build_sstmt_list builder sl env
      | SExpr e -> ignore(build_expr builder e env); builder

  in





  let build_stmt builder = function
    (* need ast to include statements *)

  