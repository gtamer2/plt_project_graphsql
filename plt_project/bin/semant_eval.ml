open Ast
open Sast
open Semantic_checker
open Printf

(* Initial empty environment *)
let empty_env = {
  binding = BindMap.empty;
  vars = VarMap.empty;
  graphs = GraphMap.empty;
}

let _ = 
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.tokenize lexbuf in
  let sexpr, new_env = Semantic_checker.check empty_env expr in
  Printf.printf "SAST:\n" (string_of_sexpr sexpr);