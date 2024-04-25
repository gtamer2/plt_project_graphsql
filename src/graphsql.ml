open Ast
open Printf
module VarMap = Map.Make(String)
(* module GraphMap = Map.Make(String) *)

(* Define a new environment type that includes both variable and graph maps *)
type environment = {
  vars: int VarMap.t;
  (* graphs: (string * Ast.graph_element list) list GraphMap.t; *)
}

(* Initial empty environment *)
let empty_env = {
  vars = VarMap.empty;
  (* graphs = GraphMap.empty; *)
}

let rec eval env = function
  | Lit(x) -> (x, env)
  | BoolLit(x) ->
      let result =  match x with
      | true -> 1
      | false -> 0 in
   (result, env)
  | Binop(e1, op, e2) ->
      let (v1, env1) = eval env e1 in
      let (v2, env2) = eval env1 e2 in
      let result = match op with
        | Add -> v1 + v2
        | Sub -> v1 - v2
        | Mul -> v1 * v2
        | Div -> v1 / v2 in
      (result, env2)
  | Seq(e1, e2) ->
      let (_, env1) = eval env e1 in
      eval env1 e2
  | Asn(var, e) ->
      let (value, env1) = eval env e in
      let env2 = VarMap.add var value env1 in
      (value, env2)
  | Var(var) ->
    (try VarMap.find var env, env 
      with not_found -> failwith (sprintf "Variable '%s' not found" var))

  let rec bool_eval env = function
  | Bool_Binop(e1, op, e2) ->
      let v1 = bool_eval env e1 in
      let v2 = bool_eval env e2 in
      match op with
      | Eq -> v1 = v2
      | Neq -> v1 <> v2
      | Gt -> v1 > v2
      | Lt -> v1 < v2
      | Gteq -> v1 >= v2
      | Lteq -> v1 <= v2
      | And -> v1 && v2
      | Or -> v1 || v2

(* let _ =
  let lexbuf = Lexing.from_channel stdin in
  (* let expr = Parser.expr Scanner.tokenize lexbuf in *)
  let expr = Parser.program Scanner.tokenize lexbuf in
  let result, _ = eval empty_env expr in
  Printf.printf "Result: %s\n" (string_of_expr expr) *)

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.tokenize lexbuf in
  let result, _ = eval VarMap.empty expr in
  print_endline (string_of_int result)