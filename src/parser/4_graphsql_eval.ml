let rec eval = function 
    Lit(x)            -> x
  | BinGraphOp(e1, op, e2) ->
      let v1  = eval e1 in
      let v2 = eval e2 in
      (match op with
	    | Intersect -> v1 + v2
      | Union -> v1 + v2
      )
      
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.tokenize lexbuf in
  let result = eval expr in
  print_endline (string_of_int result)

