
(* Semantic checking for the GraphSQL compiler *)

open Ast
open Sast

module BindMap = Map.Make(String)
module VarMap = Map.Make(String)
module GraphMap = Map.Make(String)

(* Define a new environment type that includes both variable and graph maps *)
type environment = {
  bindings: unified_type BindMap.t;
  vars: sexpr VarMap.t;
  graphs: graph_element list GraphMap.t;
}


let check init_env init_program = 
  (* let check_graph (* check graphs *)
  in  *)

  (* (* Raise an exception if the given rvalue type cannot be assigned to
      the given lvalue type *)
  let check_assign lvaluet rvaluet err =
    if lvaluet = rvaluet then lvaluet else raise (Failure err)
  in *)

  (* build local symbol table for this list of expressions *)
  let symbols = init_env.bindings in

  (* Return a variable from our symbol table *)
  let type_of_identifier s =
    try BindMap.find s symbols
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in
  
  (* return a semantically checked expression, which also constructs the environment *)
  let rec check_expr env = function
      Lit l -> ((Int, SLit l), env)
    | BoolLit l -> ((Bool, SBoolLit l), env)
    | Var var -> 
      let var_type = type_of_identifier var in
      begin match var_type with
        Typ t -> ((t, SVar var), env)
      end
    | FloatLit f -> ((Float, SFloatLit f), env)
    (* | Uniop (op, e1) ->
      let (t1, e1') = check_expr e1 in
      let err = "illegal unary operator " ^
            string_of_op op ^ " " ^ string_of_typ t1
      in
      let t = match t1 with 
          Not when t1 = Bool -> (t, SUniop(op, (t1, e1')))
        | Dot when -> (* what to do with dot operation *)
        | _ -> raise (Failure err) *)

    | Binop (e1, op, e2) as e ->
      let ((t1, e1'), env1) = check_expr env e1 in
      let ((t2, e2'), env2) = check_expr env1 e2 in
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
        ((t, SBinop((t1, e1'), op, (t2, e2'))), env2)
      else raise (Failure err)
    (* | Seq (e1, e2) -> 
      let se1, env1 = check_expr env e1 in
      let se2, env2 = check_expr env1 e2 in
      (((* what type is an seq*), SSeq (se1, se2)), env2) *)
    | Asn (var, e) ->
      (* let str = var ^ " = " ^ string_of_expr e in
        Printf.printf "variable Assignment: %s\n" str; *)
      let ((t, e'), env1)  = check_expr env e in
      (* let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                string_of_typ rt ^ " in " ^ string_of_expr exit
      in *)
        let env2 = { env1 with bindings = BindMap.add var (Typ(t)) env1.bindings } in 
        let env3 = { env2 with vars = VarMap.add var (t, e') env2.vars } in
        ((t, SAsn(var, (t, e'))), env3)
      (* | graph_element -> let env2 = { env1 with vars = GraphMap.add graph x env1.graphs } in
      | graph_element list ->
      | _,  *)
      in
  let rec check_stmt_list env = function
        [] -> ([], env)
      | stmt :: rest ->
        let (result_sstmt, new_env) = check_stmt env stmt in
        let (result_sl, new_env') = check_stmt_list new_env rest in
        (result_sstmt :: result_sl, new_env')
    and
    check_stmt env = function
      | Block sl ->
        let (sstmt_list', env') = check_stmt_list env sl in
        (SBlock(sstmt_list'), env')
      | Expr e -> 
        let (sexpr, env') = check_expr env e in
        (SExpr(sexpr), env')
      (* | If ->
      | IfElse ->
      | While ->
      | For -> *)
  
  in
  check_stmt_list init_env init_program
    (* | Graph g -> check_graph 
    | GraphAsn (var, e) -> S *)