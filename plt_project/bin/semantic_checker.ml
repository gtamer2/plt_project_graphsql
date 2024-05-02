
(* Semantic checking for the GraphSQL compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

let check () = 
  let check_vars (* check *)
  in
  let check_graph (* check graphs *)
  in 
  let check_assign lvaluet rvaluet err =
    if lvaluet = rvaluet then lvaluet else raise (Failure err)
  in

  (* Return a variable from our local symbol table *)
  let type_of_identifier s =
    try StringMap.find s symbols
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in
  
  (* return a semantically checked expression *)
  let rec check_expr = function
      Lit l -> (Int, SLit l)
    | BoolLit l -> (Bool, SBoolLit l)
    | Var var -> (type_of_identifier var, SVar var)
    | Float f -> (Float, SFloatLit f)
    | Vertex v -> 
    | Edge e ->
    | Graph g ->
    | Uniop (op, e1) ->
      let (t1, e1') = check_expr e1 in
      let err = "illegal unary operator " ^
            string_of_op op ^ " " ^ string_of_typ t1
      in
      let t = match t1 with 
          Not when t1 = Bool -> (t, SUniop(op, (t1, e1')))
        | Dot when -> (* what to do with dot operation *)
        | _ -> raise (Failure err)

    | Binop (e1, op, e2) as e ->
      let (t1, e1') = check_expr e1
      and (t2, e2') = check_expr e2 in
      let err = "illegal binary operator " ^
                string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                string_of_typ t2 ^ " in " ^ string_of_expr e
      in
      (* All binary operators require operands of the same type*)
      if t1 = t2 then
        let t = match op with 
            Add | Sub | Mul | Div | Mod when t1 = Int -> Int
          | Add | Sub | Mul | Div when t1 = Float -> Float
          | Eq | Neq -> Bool
          | Gteq | Lteq | Gt | Lt when t1 = Int || t1 = Float -> Bool
          | And | Or when t1 = Bool -> Bool
          | _ -> raise (Failure err)
        in
        (t, SBinop((t1, e1'), op, (t2, e2')))
      else raise (Failure err)
    | Seq (e1, e2) ->
    | Asn (var, e) as ex ->
      let lt = type_of_identifer var
      and (rt, e') = check_expr e in
      let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                string_of_typ rt ^ " in " ^ string_of_expr exit
      in
      (check_assign lt rt err, SAsn(var, (rt, e')))
    | GraphAsn (var, e) -> S