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
\001\000\001\000\006\000\000\000"

let yylen = "\002\000\
\004\000\008\000\003\000\001\000\000\000\003\000\004\000\005\000\
\001\000\001\000\001\000\003\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000\
\007\000\011\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\009\000\011\000\000\000\010\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\014\000\015\000\000\000\030\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\024\000\025\000\026\000\
\027\000\000\000\000\000\016\000\007\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\008\000\000\000\000\000\000\000\
\000\000\006\000\000\000\000\000\000\000\003\000\000\000\001\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\034\000\
\002\000"

let yydgoto = "\002\000\
\010\000\059\000\060\000\055\000\011\000\000\000"

let yysindex = "\007\000\
\095\255\000\000\000\000\000\000\251\254\000\000\211\254\095\255\
\236\254\202\255\254\254\095\255\240\254\249\254\150\255\095\255\
\095\255\095\255\095\255\095\255\095\255\095\255\095\255\095\255\
\095\255\095\255\095\255\095\255\095\255\095\255\019\255\222\255\
\000\000\000\000\239\254\000\000\176\255\191\000\191\000\252\254\
\252\254\222\255\096\255\245\254\245\254\000\000\000\000\000\000\
\000\000\059\255\059\255\000\000\000\000\248\254\009\255\007\255\
\012\255\013\255\016\255\022\255\000\000\095\255\046\255\053\255\
\248\254\000\000\051\255\041\255\078\255\000\000\037\255\000\000\
\079\255\049\255\048\255\095\255\094\255\081\255\070\255\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\000\000\118\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\031\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\018\000\
\000\000\000\000\000\000\000\000\000\000\142\000\161\000\149\000\
\155\000\060\000\134\000\081\000\107\000\000\000\000\000\000\000\
\000\000\051\000\127\000\000\000\000\000\080\255\000\000\000\000\
\000\000\000\000\082\255\000\000\000\000\000\000\000\000\000\000\
\080\255\000\000\000\000\000\000\000\000\000\000\061\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\016\000\000\000\054\000\000\000\000\000\000\000"

let yytablesize = 467
let yytable = "\012\000\
\013\000\014\000\022\000\025\000\026\000\027\000\028\000\001\000\
\023\000\024\000\025\000\026\000\027\000\028\000\029\000\030\000\
\016\000\012\000\033\000\034\000\053\000\054\000\031\000\015\000\
\057\000\058\000\013\000\032\000\052\000\035\000\032\000\037\000\
\038\000\039\000\040\000\041\000\042\000\043\000\044\000\045\000\
\046\000\047\000\048\000\049\000\050\000\051\000\061\000\062\000\
\063\000\064\000\028\000\017\000\018\000\019\000\020\000\068\000\
\021\000\022\000\065\000\031\000\033\000\066\000\069\000\023\000\
\024\000\025\000\026\000\027\000\028\000\029\000\030\000\023\000\
\024\000\025\000\026\000\027\000\028\000\067\000\072\000\073\000\
\022\000\017\000\018\000\019\000\020\000\074\000\021\000\022\000\
\075\000\076\000\077\000\078\000\071\000\023\000\024\000\025\000\
\026\000\027\000\028\000\029\000\030\000\079\000\003\000\004\000\
\005\000\006\000\023\000\081\000\023\000\024\000\025\000\026\000\
\027\000\028\000\029\000\030\000\007\000\036\000\070\000\005\000\
\000\000\004\000\080\000\000\000\000\000\000\000\029\000\000\000\
\000\000\000\000\000\000\008\000\000\000\021\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\017\000\009\000\000\000\
\000\000\000\000\000\000\000\000\019\000\000\000\017\000\018\000\
\019\000\020\000\020\000\021\000\022\000\000\000\000\000\000\000\
\018\000\000\000\023\000\024\000\025\000\026\000\027\000\028\000\
\029\000\030\000\000\000\000\000\000\000\000\000\000\000\000\000\
\017\000\018\000\019\000\020\000\000\000\021\000\022\000\000\000\
\000\000\000\000\000\000\036\000\023\000\024\000\025\000\026\000\
\027\000\028\000\029\000\030\000\000\000\000\000\000\000\000\000\
\000\000\000\000\017\000\018\000\019\000\020\000\000\000\021\000\
\022\000\000\000\000\000\000\000\000\000\056\000\023\000\024\000\
\025\000\026\000\027\000\028\000\029\000\030\000\017\000\018\000\
\019\000\020\000\000\000\000\000\022\000\000\000\000\000\000\000\
\000\000\000\000\023\000\024\000\025\000\026\000\027\000\028\000\
\029\000\030\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\013\000\013\000\013\000\013\000\000\000\013\000\013\000\
\000\000\000\000\000\000\000\000\000\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\000\000\000\000\012\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\032\000\
\032\000\032\000\032\000\000\000\032\000\032\000\013\000\000\000\
\000\000\000\000\013\000\032\000\032\000\032\000\032\000\032\000\
\032\000\032\000\032\000\028\000\028\000\028\000\028\000\012\000\
\028\000\028\000\000\000\012\000\000\000\033\000\033\000\033\000\
\033\000\031\000\033\000\033\000\032\000\028\000\028\000\000\000\
\032\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
\033\000\022\000\022\000\022\000\022\000\000\000\022\000\022\000\
\028\000\000\000\000\000\000\000\028\000\022\000\022\000\000\000\
\000\000\031\000\033\000\022\000\022\000\031\000\033\000\000\000\
\000\000\000\000\000\000\023\000\023\000\023\000\023\000\000\000\
\023\000\023\000\000\000\000\000\000\000\000\000\022\000\023\000\
\023\000\000\000\022\000\000\000\000\000\023\000\023\000\029\000\
\029\000\029\000\029\000\000\000\029\000\029\000\021\000\021\000\
\021\000\021\000\000\000\021\000\021\000\000\000\017\000\017\000\
\023\000\029\000\029\000\017\000\023\000\019\000\019\000\019\000\
\019\000\000\000\019\000\020\000\020\000\020\000\020\000\000\000\
\020\000\018\000\018\000\000\000\029\000\000\000\018\000\000\000\
\029\000\000\000\000\000\021\000\000\000\000\000\000\000\021\000\
\000\000\000\000\000\000\017\000\000\000\000\000\000\000\017\000\
\000\000\000\000\019\000\000\000\000\000\000\000\019\000\000\000\
\020\000\019\000\020\000\000\000\020\000\022\000\018\000\000\000\
\000\000\000\000\018\000\023\000\024\000\025\000\026\000\027\000\
\028\000\029\000\030\000"

let yycheck = "\005\001\
\000\000\047\001\007\001\015\001\016\001\017\001\018\001\001\000\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\037\001\000\000\035\001\036\001\038\001\039\001\025\001\008\000\
\033\001\034\001\032\001\012\000\010\001\037\001\000\000\016\000\
\017\000\018\000\019\000\020\000\021\000\022\000\023\000\024\000\
\025\000\026\000\027\000\028\000\029\000\030\000\038\001\041\001\
\037\001\037\001\000\000\001\001\002\001\003\001\004\001\010\001\
\006\001\007\001\043\001\000\000\000\000\040\001\010\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\013\001\
\014\001\015\001\016\001\017\001\018\001\062\000\038\001\002\001\
\000\000\001\001\002\001\003\001\004\001\049\001\006\001\007\001\
\010\001\041\001\043\001\076\000\042\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\008\001\008\001\009\001\
\010\001\011\001\000\000\038\001\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\022\001\000\000\065\000\040\001\
\255\255\040\001\042\001\255\255\255\255\255\255\000\000\255\255\
\255\255\255\255\255\255\037\001\255\255\000\000\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\000\000\048\001\255\255\
\255\255\255\255\255\255\255\255\000\000\255\255\001\001\002\001\
\003\001\004\001\000\000\006\001\007\001\255\255\255\255\255\255\
\000\000\255\255\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\255\255\255\255\255\255\255\255\255\255\255\255\
\001\001\002\001\003\001\004\001\255\255\006\001\007\001\255\255\
\255\255\255\255\255\255\038\001\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\255\255\255\255\255\255\255\255\
\255\255\255\255\001\001\002\001\003\001\004\001\255\255\006\001\
\007\001\255\255\255\255\255\255\255\255\038\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\001\001\002\001\
\003\001\004\001\255\255\255\255\007\001\255\255\255\255\255\255\
\255\255\255\255\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\255\255\006\001\007\001\
\255\255\255\255\255\255\255\255\255\255\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\255\255\255\255\006\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\001\001\
\002\001\003\001\004\001\255\255\006\001\007\001\038\001\255\255\
\255\255\255\255\042\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\001\001\002\001\003\001\004\001\038\001\
\006\001\007\001\255\255\042\001\255\255\001\001\002\001\003\001\
\004\001\006\001\006\001\007\001\038\001\019\001\020\001\255\255\
\042\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\001\001\002\001\003\001\004\001\255\255\006\001\007\001\
\038\001\255\255\255\255\255\255\042\001\013\001\014\001\255\255\
\255\255\038\001\038\001\019\001\020\001\042\001\042\001\255\255\
\255\255\255\255\255\255\001\001\002\001\003\001\004\001\255\255\
\006\001\007\001\255\255\255\255\255\255\255\255\038\001\013\001\
\014\001\255\255\042\001\255\255\255\255\019\001\020\001\001\001\
\002\001\003\001\004\001\255\255\006\001\007\001\001\001\002\001\
\003\001\004\001\255\255\006\001\007\001\255\255\001\001\002\001\
\038\001\019\001\020\001\006\001\042\001\001\001\002\001\003\001\
\004\001\255\255\006\001\001\001\002\001\003\001\004\001\255\255\
\006\001\001\001\002\001\255\255\038\001\255\255\006\001\255\255\
\042\001\255\255\255\255\038\001\255\255\255\255\255\255\042\001\
\255\255\255\255\255\255\038\001\255\255\255\255\255\255\042\001\
\255\255\255\255\038\001\255\255\255\255\255\255\042\001\255\255\
\038\001\003\001\004\001\255\255\042\001\007\001\038\001\255\255\
\255\255\255\255\042\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001"

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
# 389 "parser.ml"
               : 'graph_element))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 41 "parser.mly"
                                                       ( Edge(_3, _5, _7) )
# 398 "parser.ml"
               : 'graph_element))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'graph_element) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'graph_elements) in
    Obj.repr(
# 44 "parser.mly"
                                         ( _1::_3 )
# 406 "parser.ml"
               : 'graph_elements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'graph_element) in
    Obj.repr(
# 45 "parser.mly"
                    ( [_1] )
# 413 "parser.ml"
               : 'graph_elements))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
                ( [] )
# 419 "parser.ml"
               : 'graph_elements))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'graph_elements) in
    Obj.repr(
# 50 "parser.mly"
                           (_2)
# 426 "parser.ml"
               : 'graph_elements_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
                         ( Graph([]) )
# 432 "parser.ml"
               : 'graph_init))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'graph_elements_list) in
    Obj.repr(
# 55 "parser.mly"
                                             ( Graph(_4) )
# 439 "parser.ml"
               : 'graph_init))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 59 "parser.mly"
                 ( Lit(_1) )
# 446 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 60 "parser.mly"
               ( FloatLit(_1) )
# 453 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 61 "parser.mly"
               ( BoolLit(_1) )
# 460 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 62 "parser.mly"
                           (Asn(_1, _3))
# 468 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 63 "parser.mly"
               ( Var(_1) )
# 475 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 64 "parser.mly"
                            ( GraphAccess(_1, "vertices") )
# 482 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 65 "parser.mly"
                         ( GraphAccess(_1, "edges") )
# 489 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'graph_init) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "parser.mly"
                             ( GraphAsn(_3, _1))
# 497 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 67 "parser.mly"
                     (Binop(_1, Add, _3) )
# 505 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 68 "parser.mly"
                      ( Binop(_1, Sub, _3) )
# 513 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 69 "parser.mly"
                      ( Binop(_1, Mul, _3) )
# 521 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 70 "parser.mly"
                       ( Binop(_1, Div, _3) )
# 529 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 71 "parser.mly"
                        ( Binop(_1, Mod, _3) )
# 537 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 72 "parser.mly"
                    ( Binop(_1, Eq, _3) )
# 545 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 73 "parser.mly"
                       ( Binop(_1, Neq, _3) )
# 553 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 74 "parser.mly"
                   ( Binop(_1, Gt, _3) )
# 561 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 75 "parser.mly"
                   ( Binop(_1, Lt, _3) )
# 569 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 76 "parser.mly"
                     ( Binop(_1, Gteq, _3) )
# 577 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 77 "parser.mly"
                     ( Binop(_1, Lteq, _3) )
# 585 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 78 "parser.mly"
                    ( Binop(_1, And, _3) )
# 593 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 79 "parser.mly"
                   ( Binop(_1, Or, _3) )
# 601 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 80 "parser.mly"
                 ( _2 )
# 608 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 81 "parser.mly"
                          ( Seq(_1, _3) )
# 616 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 82 "parser.mly"
                     (_1)
# 623 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 83 "parser.mly"
                               ( print_endline "Parsing if"; If(_3, _6))
# 631 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : Ast.expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : Ast.expr) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 84 "parser.mly"
                                               ( print_endline "Parsing if/else"; IfElse(_3, _6, _10))
# 640 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 87 "parser.mly"
           ( _1 )
# 647 "parser.ml"
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
