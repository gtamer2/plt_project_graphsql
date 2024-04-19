module L = Llvm (* LLVM defined ocaml library *)
module A = Ast (* defined in previous phases *)
open Sast (* defined in previous phases *)

module StringMap = Map.Make(String)

let translate (global, functions) = 
  let cotnext = L.global_context() in (*we don't need contexts beyond global context for the final project*)

  let i32_t = L.i32_type (* context gets the corresponding type for 32 bit integer *) in
  let i1_t = L.i1_type context in (* context for LLVM type of 1 bit... used to represent bool type *)

  (* This function maps microC type in type of our LLVM context *)
  (* alternate syntax let ltype_oof_typ t = match t with A.int -> ... *)
  let ltype_of_typ = function
    A.int -> i32_t
    | A.Bool -> i1_t
  in 

  let the_module = L.create_module context "MicroC" in

  (* create global variables in the module *)
  let global_vars: L.llvalue StringMap.t =
     (*this global bar takes two parameters
        1. string map from global variable name to global variable definition in LLVM IR
        2. variable definition which conntains type and name *)
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0 in 
      (*here, we can view "the_module" as a pointer *) 
      (* this line will return an L.LLValue*)
      StringMap.add n (L.define_global n init the_module) m
    in
    List.fold_left global_var StringMap.empty global_vars
  in

  let ltype_of_bind (t, n) = ltype_of_type t in

  let function_decls: (L.llvalue * sfunc_def) StringMap.t =
    (* similar to global_vars, we need the singular *)
    (* fdecl here is from AST *)
    let function_decl m fdecl = 
      let name = fdecl.sfname in
      let formal_types = Array.of_list (List.map ltype_of_bind fdecl.sformals) in
      let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in (* function signature*) 
         (*two args: 
            first is return type of function taken from AST and converted to LLVM type
            second is array of formal types of parameters *)
      StringMap.add name (L.define_function name ftype the_module, fdecl) m
    in
    List.fold_left function_decl StringMap.empty functions
  in

  (*TODO: construct local vars table *)
  let local_vars = ... in
  let lookup n = try StringMap.find n local_vars
    with Not_found StringMap.find n global_vars
  in

  (* now we want to fill in the body of the function. A tthis point we have global vars and function signnatures *)
  (* First we need a helper util to build a signle expression *)
  let rec build_expr builder ((_, e) : sexpr) = 
    match e with
      SLiteral i -> L.const_int i32_t i
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SId s -> L.build_load (lookup s) s builder
      | SAssign (s, e) -> let e_addr = build_expr builder e in
      ignore(L.build_store e_addr (lookup s) builder); e_addr
      | SBinop (e1, op, e2) ->
        let e1_addr = build_expr builder e1 in
        let e2_addr = build_expr builder e2 in
        let l_op = match op with
          A.add -> L.build_add
          | A.sub -> L.build_sub
          | A.And -> L.build_and
          | _ -> L.build_or (* TODO not complete *)
        in
        l_op e1_addr e2_addr "tmp" builder

      | SCall (f, ags) -> 
        let (fdef, fast) = StringMap.find f function_decls in
        let llargs = List.map (build_expr builder) (List.rev args)
        L.build_call fdef (Array.of_list (List.rev llargs)) (f^"_result") builder
    in

  (* Then  we need a main function to build entire blocks *)
  let build_function_body fdecl = 
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in (* pass AST of this function which contains list of statements *)
      (* NEW CONCEPT "BUILDER". We can view it as a "cursor" that determines where IR is inserted. *)
      (* reminder, "the_function" here is a pointer *)
      (* L.builder_at_end takes 
         1 - a context
         2 - poiner?? TODO cofirm  *)
      let builder = L.builder_at_end context (L.entry_block the_function) in (* get the position where we want to insert the generated IR *)
      (* now we have the builder. Lets build the statements *)

      let add_terminal block terminal = 
        match L.block_termrinator block with 
        Some _ -> () (*do nothingn if some terrminal already there*)
        | None -> ignore (terminal) in (*if no terminal already, add it*)
      let rec build_stmt builder = function
        Sblock sl -> List.fold_left build_stmt builder sl (* recurse - note Sblock is a list of statements*) 
        | SExpr e -> ignore (build_expr builder e); builder (* want to build e.code and e.address. 
           Want this to return a builder. 
           but the first statement rerturns {builder, address}, so we do that custom ignorer trick*)
        | SReturn e -> ignore(L.build_ret (build_expr builder e) builder); builder
        | SIf (e, then_stmt, else_stmt) -> 
        (*
          E.code
          <conditional jump>
          then:
            then_stmt
            jmp end
          else:
            else_stmt
            jmp end
          end:

        *)
          let e_addr = build_expr builder e in (*E.code*)
          let then_bb = L.append_bb context "then" the_function in (*LLVM will check if we have the then label in our function... if so, it will be something like then1, then2, ... *)
          let else_bb = L.append_bb context "else" the_function in
          let end_bb = L.append_bb context "if_end" the_function in
          
          (* the scaffolding... *)
          ignore(L.build_cond_br e_addr then_bb else_bb builder);
  
          (* the body... *)
          (* we need to insnert into each scaffolding block *)
          ignore(build_stmt (L.builder_at_end context then_bb) then_stmt);
          add_terrminal then_bb (L.build_br end_bb (L.builder_at_end context then_bb));
        
          (* final scaffolding *)
          ignore(L.build_br end_bb (L.builder_at_end context then_bb))

          ignore(build_stmt (L.builder_at_end context else_bb) else_stmt);
          add_terrminal else_bb (L.build_br end_bb (L.builder_at_end context else_bb));

          L.builder_at_end context end_bb
        | SWhile (e, body) -> 
          (*
            sdsdd
            <- builder
            jmp while
            
            while:
              e.code

              cond_br e.addr body end

            body:
              body_stmt
              jmp while


            end:
          *)
          let while_bb = L.append_block context "while" the_function in
          ignore(L.build_br whiile_bb builder);

          let e_addr = build_expr (L.builder_at_end context while_bb) e
          let body_bb = L.append_block context "body" the_function in
          let end_bb = L.append_block context "while_end" the_function in
          ignore(L.build_cond_br e_addr body_bb end_bb (L.builder_at_end context while_bb));

          ignore(build_stmt (L.builder_at_end context body_bb) body)
          add_terminal body_bb (L.build_br while_bb (L.builder_at_end context body_bb))

          L.builder_at_end context end_bb
      in

      let func_builder = build_stmt builder (SBlock fdecl.sbody) in

  in 

  (* take builder and statement as args, and return
      a builder with IR code for given statement inserted/cursor moved *)
  List.iter build_function_body functions; (*note: List.iter applies and returns void*)
  the_module


(* answer to why LLVM is not cheating:: remember IR algorithm -- we want to make more modular -- what the IR code will be like should be  *)
(* we provide enough info to LLVM, and how the code will be printed out is decoupled *)