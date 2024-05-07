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

let varmap_to_str m = 
  let inners = List.map (fun (k, v) -> k ^ " -> " ^ (
    match v with 
    | (_, SLit i) -> string_of_int i
    | (_, SFloatLit f) -> string_of_float f
    | other -> string_of_sexpr other
  )) (VarMap.bindings m)
  in "[" ^ (String.concat ", " inners) ^ "]"

let bindmap_to_str m = 
  let inners = List.map (fun (k, v) -> k ^ " -> " ^ (
    begin match v with 
      | Int -> string_of_typ v
      | Bool -> string_of_typ v
      | Float -> string_of_typ v
      | String -> string_of_typ v
      | GraphType(v) -> string_of_typ (GraphType v)
    end
    )) (BindMap.bindings m)
  in "[" ^ (String.concat ", " inners) ^ "]"

let _ = 
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.stmt_list Scanner.tokenize lexbuf in
  let sprogram, new_env = Semantic_checker.check empty_env program in
  let sprogram_string = String.concat "" (List.map string_of_sstmt sprogram) in
  let svarmap_string = varmap_to_str new_env.vars in
  let sbindmap_string = bindmap_to_str new_env.bindings in
  Printf.printf "SAST: \n%s\n%s\n%s" sprogram_string svarmap_string sbindmap_string;