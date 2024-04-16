open Ast
open Printf
module StringMap = Map.Make(String)

let rec eval mymap = function 
    Literal(x)  -> x
  (*| FloatLit(x) -> x*)
  | Sequence(e1, e2) -> 
      let _ = eval mymap e1 in 
      eval mymap e2
  | Assign(id, value) -> 
      let v = eval mymap value in
      let tmp = StringMap.add id v mymap in
      eval tmp (Variable id)
  | Binop(e1, op, e2) ->
      let v1  = eval mymap e1 in
      let v2 = eval mymap e2 in
      let result = match op with
	  | Add -> v1 + v2
      | Sub -> v1 - v2
      | Mult -> v1 * v2
      | Divd -> v1 / v2
      | Mod -> v1 mod v2
  in result 
  | Variable (var) ->
      (try StringMap.find var mymap 
      with not_found -> failwith (sprintf "Variable '%s' not found" var))

  let _ =
    let lexbuf = Lexing.from_channel stdin in
          try
                let program = Parser.expr Scanner.tokenize lexbuf in
                let mymap = StringMap.empty in
                let result = eval mymap program in
                Printf.printf "%d\n" result; 
                (*print_endline (string_of_int result);*)
                flush stdout;
          with
          | Parsing.Parse_error ->
                Printf.eprintf "Syntax error at position %d\n" (Lexing.lexeme_start lexbuf);
                exit (-1)
          | Failure(msg) ->
                Printf.eprintf "Error: %s\n" msg;
                exit (-1) 

(*

open Ast

module VarMap = Map.Make(String)

let rec eval env = function
    | Literal(x) -> (x, env)
    | Binop(e1, op, e2) ->
        let (v1, env1) = eval env e1 in
        let (v2, env2) = eval env1 e2 in
        let result = match op with
        | Add -> v1 + v2
        | Sub -> v1 - v2
        | Mult -> v1 * v2
        | Divd -> v1 / v2 in
        (result, env2)
    | Seq(e1, e2) ->
        let (_, env1) = eval env e1 in
        eval env1 e2
    | Assign(var, e) ->
        let (value, env1) = eval env e in
        let env2 = VarMap.add var value env1 in
        (value, env2)
    | Variable(var) ->
        VarMap.find var env, env  


let _ =
    let lexbuf = Lexing.from_channel stdin in
    let expr = Parser.expr Scanner.tokenize lexbuf in
    let result, _ = eval VarMap.empty expr in
    print_endline (string_of_int result)
*)