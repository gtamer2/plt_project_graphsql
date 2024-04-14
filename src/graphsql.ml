open Ast
open Printf
module StringMap = Map.Make(String)

let rec eval mymap = function 
    Literal(x)  -> x
  (*| FloatLit(x) -> x*)
  | Seq(e1, e2) -> 
      let _ = eval mymap e1 in 
      eval mymap e2
 | Assign(id, value) -> 
      let v = eval mymap value in
      let tmp = StringMap.add id v mymap in
      eval tmp (Variable id)
  | Binop(e1, op, e2) ->
      let v1  = eval mymap e1 in
      let v2 = eval mymap e2 in
      (match op with
	  Add -> v1 + v2
      | Sub -> v1 - v2
      | Mult -> v1 * v2
      | Divd -> v1 / v2
      | Mod -> v1 mod v2)
  | Variable (var) ->
      (try StringMap.find var mymap 
      with not_found -> failwith (sprintf "Variable '%s' not found" var))
      (** only for a*b+c*)

  let _ =
    let lexbuf = Lexing.from_channel stdin in
      (**try*)
      while true do
          try
                let program = Parser.expr Scanner.tokenize lexbuf in
                let mymap = StringMap.empty in
                let result = eval mymap program in
                print_endline (string_of_int result)
          with
          | Parsing.Parse_error ->
                Printf.eprintf "Syntax error at position %d\n" (Lexing.lexeme_start lexbuf);
                exit (-1)
          | Failure(msg) ->
                Printf.eprintf "Error: %s\n" msg;
                exit (-1)
       done