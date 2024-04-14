open Ast
open Printf
module StringMap = Map.Make(String)

let rec eval mymap = function 
    Lit(x) -> x
  | Seq(e1, e2) -> 
      let _ = eval mymap e1 in 
      eval mymap e2
 | Asn(id, value) -> 
      let v = eval mymap value in
      let tmp = StringMap.add id v mymap in
      eval tmp (Var id)
  | Binop(e1, op, e2) ->
      let v1  = eval mymap e1 in
      let v2 = eval mymap e2 in
      (match op with
	      Add -> v1 + v2
      | Sub -> v1 - v2
      | Mul -> v1 * v2
      | Div -> v1 / v2)
  | Var (id) ->
      (try StringMap.find id mymap 
      with not_found -> failwith (sprintf "Variable '%s' not found" id))
      (** only for a*b+c*)

let _ =
let lexbuf = Lexing.from_channel stdin in
  (**try*)
  while true do
      try
            let expr = Parser.expr Scanner.tokenize lexbuf in
            let mymap = StringMap.empty in
            let result = eval mymap expr in
            print_endline (string_of_int result)
      with
      | Parsing.Parse_error ->
            Printf.eprintf "Syntax error at position %d\n" (Lexing.lexeme_start lexbuf);
            exit (-1)
      | Failure(msg) ->
            Printf.eprintf "Error: %s\n" msg;
            exit (-1)
   done
   (*
with
| End_of_file -> exit 0 (* Exit gracefully on end-of-file *)*)




let map_to_str m = 
  let inners = List.map (fun (k, v) -> k ^ " -> " ^ (string_of_int v)) (StringMap.bindings m)
  in "[" ^ (String.concat ", " inners) ^ "]"

let rec eval_stmt stmt mp = match stmt with
    | Expr(expr) -> eval_expr expr mp
  and eval_expr expr mp = match expr with
    | Literal(x) -> x, mp
    | Assign(id, vl) -> 
      let v1, mp1 = eval_expr vl mp in
      let mp2 = StringMap.add id v1 mp1 in
      v1, mp2
    | Variable(id) ->
      let vl = StringMap.find id mp in
      vl, mp

let rec eval program mp = match program with 
  | [] -> program, mp
  | stmt :: stmt_list -> 
    let stmt_result, mp = eval_stmt stmt mp in 
    eval stmt_list mp


  let _ =
    let lexbuf = Lexing.from_channel stdin in
      (**try*)
      while true do
          try
                let program = Parser.program Scanner.tokenize lexbuf in
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