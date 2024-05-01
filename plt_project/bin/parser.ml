type token =
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | SEMICOLON
  | MODULUS
  | EOF
  | LITERAL of (
# 7 "parser.mly"
        int
# 14 "parser.ml"
)
  | BLIT of (
# 8 "parser.mly"
        bool
# 19 "parser.ml"
)
  | VARIABLE of (
# 9 "parser.mly"
        string
# 24 "parser.ml"
)
  | FLOATLIT of (
# 10 "parser.mly"
        float
# 29 "parser.ml"
)
  | STRINGLIT of (
# 11 "parser.mly"
        string
# 34 "parser.ml"
)
  | EQL
  | NOTEQL
  | GT
  | LT
  | GTEQ
  | LTEQ
  | AND
  | OR
  | NOT
  | CREATE
  | SELECT
  | FROM
  | AS
  | WHERE
  | INSERT
  | UNION
  | INTERSECT
  | APPLY
  | WHILE
  | DOT
  | VERTEX
  | EDGE
  | VERTICES
  | EDGES
  | LP
  | RP
  | LB
  | RB
  | LC
  | RC
  | COMMA
  | ARROW
  | QUOTES
  | COMMENT
  | GRAPH
  | IF
  | ELSE
  | ELIF
  | DEFINE
  | FUNCTION

open Parsing
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 81 "parser.ml"
let yytransl_const = [|
  257 (* PLUS *);
  258 (* MINUS *);
  259 (* TIMES *);
  260 (* DIVIDE *);
  261 (* ASSIGN *);
  262 (* SEMICOLON *);
  263 (* MODULUS *);
    0 (* EOF *);
  269 (* EQL *);
  270 (* NOTEQL *);
  271 (* GT *);
  272 (* LT *);
  273 (* GTEQ *);
  274 (* LTEQ *);
  275 (* AND *);
  276 (* OR *);
  277 (* NOT *);
  278 (* CREATE *);
  279 (* SELECT *);
  280 (* FROM *);
  281 (* AS *);
  282 (* WHERE *);
  283 (* INSERT *);
  284 (* UNION *);
  285 (* INTERSECT *);
  286 (* APPLY *);
  287 (* WHILE *);
  288 (* DOT *);
  289 (* VERTEX *);
  290 (* EDGE *);
  291 (* VERTICES *);
  292 (* EDGES *);
  293 (* LP *);
  294 (* RP *);
  295 (* LB *);
  296 (* RB *);
  297 (* LC *);
  298 (* RC *);
  299 (* COMMA *);
  300 (* ARROW *);
  301 (* QUOTES *);
  302 (* COMMENT *);
  303 (* GRAPH *);
  304 (* IF *);
  305 (* ELSE *);
  306 (* ELIF *);
  307 (* DEFINE *);
  308 (* FUNCTION *);
    0|]

let yytransl_block = [|
  264 (* LITERAL *);
  265 (* BLIT *);
  266 (* VARIABLE *);
  267 (* FLOATLIT *);
  268 (* STRINGLIT *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\003\000\003\000\003\000\004\000\005\000\005\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\006\000\000\000"

let yylen = "\002\000\
\004\000\008\000\003\000\001\000\000\000\003\000\004\000\005\000\
\001\000\001\000\001\000\003\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000\
\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\009\000\011\000\000\000\010\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\014\000\015\000\
\000\000\030\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\024\000\025\000\026\000\027\000\000\000\000\000\
\016\000\007\000\000\000\000\000\000\000\000\000\000\000\000\000\
\008\000\000\000\000\000\000\000\006\000\000\000\000\000\003\000\
\001\000\000\000\000\000\000\000\000\000\002\000"

let yydgoto = "\002\000\
\009\000\055\000\056\000\052\000\010\000\000\000"

let yysindex = "\255\255\
\250\254\000\000\000\000\000\000\001\255\000\000\216\254\250\254\
\062\255\005\255\250\254\238\254\253\254\035\255\250\254\250\254\
\250\254\250\254\250\254\250\254\250\254\250\254\250\254\250\254\
\250\254\250\254\250\254\250\254\009\255\102\255\000\000\000\000\
\246\254\000\000\144\255\144\255\158\255\158\255\102\255\251\254\
\007\255\007\255\000\000\000\000\000\000\000\000\043\255\043\255\
\000\000\000\000\010\255\002\255\254\254\008\255\245\254\006\255\
\000\000\052\255\060\255\010\255\000\000\033\255\070\255\000\000\
\000\000\064\255\040\255\076\255\047\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\086\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\027\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\020\000\000\000\000\000\
\000\000\000\000\102\000\132\000\021\000\125\000\026\000\114\000\
\047\000\067\000\000\000\000\000\000\000\000\000\087\000\094\000\
\000\000\000\000\048\255\000\000\000\000\000\000\049\255\000\000\
\000\000\000\000\000\000\048\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\118\000\000\000\030\000\000\000\000\000\000\000"

let yytablesize = 426
let yytable = "\001\000\
\013\000\003\000\004\000\005\000\006\000\011\000\013\000\021\000\
\022\000\023\000\024\000\025\000\026\000\027\000\028\000\007\000\
\031\000\032\000\049\000\012\000\019\000\023\000\024\000\025\000\
\026\000\031\000\032\000\050\000\051\000\029\000\008\000\060\000\
\012\000\033\000\058\000\015\000\016\000\017\000\018\000\057\000\
\019\000\020\000\053\000\054\000\059\000\061\000\022\000\021\000\
\022\000\023\000\024\000\025\000\026\000\027\000\028\000\021\000\
\022\000\023\000\024\000\025\000\026\000\062\000\015\000\016\000\
\017\000\018\000\023\000\019\000\020\000\063\000\065\000\066\000\
\034\000\067\000\021\000\022\000\023\000\024\000\025\000\026\000\
\027\000\028\000\068\000\069\000\070\000\034\000\028\000\005\000\
\004\000\064\000\000\000\000\000\000\000\029\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\017\000\015\000\016\000\
\017\000\018\000\000\000\000\000\020\000\000\000\000\000\000\000\
\000\000\021\000\021\000\022\000\023\000\024\000\025\000\026\000\
\027\000\028\000\000\000\000\000\020\000\014\000\000\000\000\000\
\030\000\000\000\000\000\018\000\035\000\036\000\037\000\038\000\
\039\000\040\000\041\000\042\000\043\000\044\000\045\000\046\000\
\047\000\048\000\017\000\018\000\000\000\000\000\020\000\000\000\
\000\000\000\000\000\000\000\000\021\000\022\000\023\000\024\000\
\025\000\026\000\027\000\028\000\020\000\000\000\000\000\000\000\
\000\000\000\000\021\000\022\000\023\000\024\000\025\000\026\000\
\027\000\028\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\013\000\013\000\013\000\013\000\000\000\013\000\013\000\
\000\000\000\000\000\000\000\000\000\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\019\000\019\000\019\000\
\019\000\012\000\019\000\032\000\032\000\032\000\032\000\031\000\
\032\000\032\000\000\000\000\000\000\000\000\000\013\000\032\000\
\032\000\032\000\032\000\032\000\032\000\032\000\032\000\022\000\
\022\000\022\000\022\000\000\000\022\000\022\000\000\000\000\000\
\000\000\012\000\019\000\022\000\022\000\000\000\000\000\031\000\
\032\000\022\000\022\000\023\000\023\000\023\000\023\000\000\000\
\023\000\023\000\000\000\000\000\000\000\000\000\000\000\023\000\
\023\000\000\000\000\000\000\000\022\000\023\000\023\000\028\000\
\028\000\028\000\028\000\000\000\028\000\028\000\029\000\029\000\
\029\000\029\000\000\000\029\000\029\000\000\000\017\000\017\000\
\023\000\028\000\028\000\017\000\000\000\000\000\000\000\000\000\
\029\000\029\000\021\000\021\000\021\000\021\000\000\000\021\000\
\021\000\000\000\000\000\000\000\028\000\020\000\020\000\020\000\
\020\000\000\000\020\000\029\000\018\000\018\000\000\000\000\000\
\000\000\018\000\000\000\017\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\021\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\020\000\000\000\000\000\000\000\000\000\000\000\
\000\000\018\000"

let yycheck = "\001\000\
\000\000\008\001\009\001\010\001\011\001\005\001\047\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\022\001\
\035\001\036\001\010\001\000\000\000\000\015\001\016\001\017\001\
\018\001\000\000\000\000\038\001\039\001\025\001\037\001\043\001\
\032\001\037\001\037\001\001\001\002\001\003\001\004\001\038\001\
\006\001\007\001\033\001\034\001\037\001\040\001\000\000\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\013\001\
\014\001\015\001\016\001\017\001\018\001\010\001\001\001\002\001\
\003\001\004\001\000\000\006\001\007\001\010\001\038\001\002\001\
\038\001\010\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\043\001\008\001\038\001\000\000\000\000\040\001\
\040\001\060\000\255\255\255\255\255\255\000\000\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\000\000\001\001\002\001\
\003\001\004\001\255\255\255\255\007\001\255\255\255\255\255\255\
\255\255\000\000\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\255\255\255\255\000\000\008\000\255\255\255\255\
\011\000\255\255\255\255\000\000\015\000\016\000\017\000\018\000\
\019\000\020\000\021\000\022\000\023\000\024\000\025\000\026\000\
\027\000\028\000\003\001\004\001\255\255\255\255\007\001\255\255\
\255\255\255\255\255\255\255\255\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\007\001\255\255\255\255\255\255\
\255\255\255\255\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\255\255\006\001\007\001\
\255\255\255\255\255\255\255\255\255\255\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\001\001\002\001\003\001\
\004\001\006\001\006\001\001\001\002\001\003\001\004\001\006\001\
\006\001\007\001\255\255\255\255\255\255\255\255\038\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\001\001\
\002\001\003\001\004\001\255\255\006\001\007\001\255\255\255\255\
\255\255\038\001\038\001\013\001\014\001\255\255\255\255\038\001\
\038\001\019\001\020\001\001\001\002\001\003\001\004\001\255\255\
\006\001\007\001\255\255\255\255\255\255\255\255\255\255\013\001\
\014\001\255\255\255\255\255\255\038\001\019\001\020\001\001\001\
\002\001\003\001\004\001\255\255\006\001\007\001\001\001\002\001\
\003\001\004\001\255\255\006\001\007\001\255\255\001\001\002\001\
\038\001\019\001\020\001\006\001\255\255\255\255\255\255\255\255\
\019\001\020\001\001\001\002\001\003\001\004\001\255\255\006\001\
\007\001\255\255\255\255\255\255\038\001\001\001\002\001\003\001\
\004\001\255\255\006\001\038\001\001\001\002\001\255\255\255\255\
\255\255\006\001\255\255\038\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\038\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\038\001\255\255\255\255\255\255\255\255\255\255\
\255\255\038\001"

let yynames_const = "\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  SEMICOLON\000\
  MODULUS\000\
  EOF\000\
  EQL\000\
  NOTEQL\000\
  GT\000\
  LT\000\
  GTEQ\000\
  LTEQ\000\
  AND\000\
  OR\000\
  NOT\000\
  CREATE\000\
  SELECT\000\
  FROM\000\
  AS\000\
  WHERE\000\
  INSERT\000\
  UNION\000\
  INTERSECT\000\
  APPLY\000\
  WHILE\000\
  DOT\000\
  VERTEX\000\
  EDGE\000\
  VERTICES\000\
  EDGES\000\
  LP\000\
  RP\000\
  LB\000\
  RB\000\
  LC\000\
  RC\000\
  COMMA\000\
  ARROW\000\
  QUOTES\000\
  COMMENT\000\
  GRAPH\000\
  IF\000\
  ELSE\000\
  ELIF\000\
  DEFINE\000\
  FUNCTION\000\
  "

let yynames_block = "\
  LITERAL\000\
  BLIT\000\
  VARIABLE\000\
  FLOATLIT\000\
  STRINGLIT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 40 "parser.mly"
                            ( Vertex(_3) )
# 373 "parser.ml"
               : 'graph_element))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 41 "parser.mly"
                                                       ( Edge(_3, _5, _7) )
# 382 "parser.ml"
               : 'graph_element))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'graph_element) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'graph_elements) in
    Obj.repr(
# 44 "parser.mly"
                                         ( _1::_3 )
# 390 "parser.ml"
               : 'graph_elements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'graph_element) in
    Obj.repr(
# 45 "parser.mly"
                    ( [_1] )
# 397 "parser.ml"
               : 'graph_elements))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
                ( [] )
# 403 "parser.ml"
               : 'graph_elements))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'graph_elements) in
    Obj.repr(
# 50 "parser.mly"
                           (_2)
# 410 "parser.ml"
               : 'graph_elements_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
                         ( Graph([]) )
# 416 "parser.ml"
               : 'graph_init))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'graph_elements_list) in
    Obj.repr(
# 55 "parser.mly"
                                             ( Graph(_4) )
# 423 "parser.ml"
               : 'graph_init))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 59 "parser.mly"
                 ( Lit(_1) )
# 430 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 60 "parser.mly"
               ( FloatLit(_1) )
# 437 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 61 "parser.mly"
               ( BoolLit(_1) )
# 444 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 62 "parser.mly"
                           (Asn(_1, _3))
# 452 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 63 "parser.mly"
               ( Var(_1) )
# 459 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 64 "parser.mly"
                            ( GraphAccess(_1, "vertices") )
# 466 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 65 "parser.mly"
                         ( GraphAccess(_1, "edges") )
# 473 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'graph_init) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "parser.mly"
                             ( GraphAsn(_3, _1))
# 481 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 67 "parser.mly"
                     (Binop(_1, Add, _3) )
# 489 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 68 "parser.mly"
                      ( Binop(_1, Sub, _3) )
# 497 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 69 "parser.mly"
                      ( Binop(_1, Mul, _3) )
# 505 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 70 "parser.mly"
                       ( Binop(_1, Div, _3) )
# 513 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 71 "parser.mly"
                        ( Binop(_1, Mod, _3) )
# 521 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 72 "parser.mly"
                    ( Binop(_1, Eq, _3) )
# 529 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 73 "parser.mly"
                       ( Binop(_1, Neq, _3) )
# 537 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 74 "parser.mly"
                   ( Binop(_1, Gt, _3) )
# 545 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 75 "parser.mly"
                   ( Binop(_1, Lt, _3) )
# 553 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 76 "parser.mly"
                     ( Binop(_1, Gteq, _3) )
# 561 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 77 "parser.mly"
                     ( Binop(_1, Lteq, _3) )
# 569 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 78 "parser.mly"
                    ( Binop(_1, And, _3) )
# 577 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 79 "parser.mly"
                   ( Binop(_1, Or, _3) )
# 585 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 80 "parser.mly"
                 ( _2 )
# 592 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 81 "parser.mly"
                          ( Seq(_1, _3) )
# 600 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 82 "parser.mly"
                     (_1)
# 607 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 85 "parser.mly"
           ( _1 )
# 614 "parser.ml"
               : 'entry))
(* Entry expr *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let expr (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.expr)
