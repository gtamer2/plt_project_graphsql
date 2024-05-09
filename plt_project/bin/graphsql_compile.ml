(* Top-level of the GraphSQL compiler: scan & parse the input,
   check the resulting AST and generate an SAST from it, generate LLVM IR,
   and dump the module *)

   type action = Ast | Sast | LLVM_IR

   let () =
     let action = ref LLVM_IR in
     let set_action a () = action := a in
     let speclist = [
       ("-a", Arg.Unit (set_action Ast), "Print the AST");
       ("-s", Arg.Unit (set_action Sast), "Print the SAST");
       ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
     ] in
     let usage_msg = "usage: ./microc.native [-a|-s|-l] [file.mc]" in
     let channel = ref stdin in
     Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;
   
     let lexbuf = Lexing.from_channel !channel in

     let stmt_list = Parser.stmt_list Scanner.tokenize lexbuf in
     match !action with
       Ast -> print_string (Ast.string_of_stmt_list stmt_list)
     | _ -> let sast = Semantic_checker.check stmt_list in
       match !action with
         Ast     -> ()
         (* Commented out bc just for debugging... don't need for now *)
      (* | Sast -> List.map Sast.string_of_sstmt stmt_list |> List.iter print_string *)
       | LLVM_IR -> print_string (Llvm.string_of_llmodule (Irgen.translate sast))
       | _ -> "Action not allowed"
   