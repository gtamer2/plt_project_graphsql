(* let rec eval = function 
    Lit(x)            -> x
  | BinGraphOp(e1, op, e2) ->
      let v1  = eval e1 in
      let v2 = eval e2 in
      (match op with
	    | Intersect -> v1 + v2
      | Union -> v1 + v2
      ) *)
      
open Ast

module StringMap = Map.Make(String)

let m = StringMap.empty

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
  let program = Parser.program Scanner.tokenize lexbuf in
  let result, mp = eval program m in
  print_endline (map_to_str mp)

