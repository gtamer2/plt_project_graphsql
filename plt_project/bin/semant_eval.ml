open Ast
open Sast
open Semantic_checker
open Printf

(* Initial empty environment *)
let empty_env = {
  bindings = BindMap.empty;
  vars = VarMap.empty;
  graphs = GraphMap.empty;
}

let _ = 
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.stmt_list Scanner.tokenize lexbuf in
  let sprogram, new_env = Semantic_checker.check empty_env program in
  let sprogram_string = String.concat "" (List.map string_of_sstmt sprogram) in
  Printf.printf "SAST: \n%s" sprogram_string;