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
  | INTO
  | DELETE
  | UNION
  | INTERSECT
  | APPLY
  | WHILE
  | QUOTES
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
# 83 "parser.ml"
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
  284 (* INTO *);
  285 (* DELETE *);
  286 (* UNION *);
  287 (* INTERSECT *);
  288 (* APPLY *);
  289 (* WHILE *);
  290 (* QUOTES *);
  291 (* DOT *);
  292 (* VERTEX *);
  293 (* EDGE *);
  294 (* VERTICES *);
  295 (* EDGES *);
  296 (* LP *);
  297 (* RP *);
  298 (* LB *);
  299 (* RB *);
  300 (* LC *);
  301 (* RC *);
  302 (* COMMA *);
  303 (* ARROW *);
  304 (* COMMENT *);
  305 (* GRAPH *);
  306 (* IF *);
  307 (* ELSE *);
  308 (* ELIF *);
  309 (* DEFINE *);
  310 (* FUNCTION *);
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
\005\000\005\000\001\000\001\000\006\000\006\000\006\000\006\000\
\006\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\008\000\000\000"

let yylen = "\002\000\
\006\000\012\000\003\000\001\000\000\000\003\000\004\000\005\000\
\006\000\006\000\000\000\002\000\002\000\003\000\007\000\011\000\
\007\000\001\000\001\000\001\000\003\000\001\000\003\000\003\000\
\003\000\004\000\004\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\018\000\020\000\000\000\019\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\043\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\012\000\000\000\
\000\000\000\000\000\000\013\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\023\000\024\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\041\000\014\000\000\000\025\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\035\000\036\000\037\000\038\000\
\000\000\000\000\007\000\000\000\000\000\000\000\026\000\000\000\
\000\000\000\000\006\000\027\000\000\000\000\000\008\000\000\000\
\000\000\000\000\000\000\003\000\000\000\000\000\009\000\010\000\
\000\000\000\000\000\000\000\000\000\000\000\000\017\000\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\016\000\000\000\000\000\002\000"

let yydgoto = "\002\000\
\015\000\054\000\055\000\025\000\016\000\017\000\018\000\000\000"

let yysindex = "\013\000\
\253\254\000\000\000\000\000\000\004\255\000\000\210\254\008\255\
\249\254\252\254\251\254\218\255\253\254\000\255\000\000\017\255\
\253\254\051\000\218\255\015\255\027\255\016\255\060\255\040\255\
\055\255\218\255\071\255\037\255\218\255\109\255\000\000\218\255\
\218\255\218\255\218\255\000\000\218\255\218\255\218\255\218\255\
\218\255\218\255\218\255\218\255\218\255\235\255\000\000\000\000\
\072\255\087\255\252\254\057\255\088\255\083\255\102\255\121\255\
\091\255\000\000\000\000\120\255\000\000\069\000\069\000\083\000\
\083\000\091\000\084\255\084\255\000\000\000\000\000\000\000\000\
\217\255\217\255\000\000\089\255\124\255\125\255\000\000\116\255\
\129\255\040\255\000\000\000\000\131\255\134\255\000\000\158\255\
\169\255\174\255\175\255\000\000\253\254\253\254\000\000\000\000\
\146\255\152\255\142\255\143\255\153\255\191\255\000\000\144\255\
\000\000\162\255\154\255\190\255\253\254\167\255\157\255\159\255\
\000\000\195\255\166\255\000\000"

let yyrindex = "\000\000\
\208\000\000\000\000\000\000\000\042\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\165\255\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\000\000\000\000\173\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\011\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\200\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\150\255\156\255\030\255\
\114\255\062\255\009\255\140\255\000\000\000\000\000\000\000\000\
\163\255\170\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\173\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\165\255\165\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\000\000\000\000\000\000\000\000\165\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\243\255\000\000\162\000\232\255\000\000\000\000\180\000\000\000"

let yytablesize = 367
let yytable = "\028\000\
\015\000\011\000\021\000\031\000\003\000\004\000\005\000\006\000\
\019\000\033\000\033\000\033\000\033\000\001\000\033\000\033\000\
\021\000\022\000\007\000\008\000\023\000\033\000\033\000\009\000\
\076\000\010\000\079\000\033\000\033\000\011\000\030\000\030\000\
\030\000\030\000\026\000\030\000\012\000\024\000\020\000\029\000\
\013\000\030\000\022\000\022\000\022\000\022\000\014\000\022\000\
\022\000\033\000\050\000\021\000\047\000\048\000\022\000\022\000\
\022\000\022\000\022\000\022\000\022\000\022\000\032\000\032\000\
\032\000\032\000\049\000\032\000\032\000\051\000\030\000\032\000\
\033\000\034\000\035\000\052\000\053\000\037\000\056\000\099\000\
\100\000\059\000\022\000\038\000\039\000\040\000\041\000\042\000\
\043\000\044\000\045\000\032\000\033\000\034\000\035\000\111\000\
\080\000\037\000\040\000\041\000\042\000\043\000\032\000\038\000\
\039\000\040\000\041\000\042\000\043\000\044\000\045\000\058\000\
\075\000\024\000\031\000\031\000\031\000\031\000\061\000\031\000\
\032\000\033\000\034\000\035\000\077\000\078\000\037\000\081\000\
\082\000\087\000\084\000\085\000\038\000\039\000\040\000\041\000\
\042\000\043\000\044\000\045\000\034\000\034\000\034\000\034\000\
\083\000\034\000\034\000\088\000\089\000\090\000\028\000\028\000\
\034\000\034\000\031\000\028\000\029\000\029\000\034\000\034\000\
\086\000\029\000\091\000\039\000\039\000\039\000\039\000\095\000\
\039\000\039\000\040\000\040\000\040\000\040\000\093\000\040\000\
\040\000\094\000\096\000\101\000\034\000\039\000\039\000\097\000\
\098\000\102\000\103\000\104\000\040\000\040\000\028\000\027\000\
\106\000\105\000\107\000\108\000\029\000\109\000\046\000\110\000\
\112\000\113\000\115\000\039\000\114\000\057\000\116\000\011\000\
\060\000\011\000\040\000\062\000\063\000\064\000\065\000\005\000\
\066\000\067\000\068\000\069\000\070\000\071\000\072\000\073\000\
\074\000\003\000\004\000\005\000\006\000\038\000\039\000\040\000\
\041\000\042\000\043\000\032\000\033\000\034\000\035\000\007\000\
\008\000\037\000\004\000\092\000\009\000\000\000\010\000\038\000\
\039\000\040\000\041\000\042\000\043\000\044\000\045\000\000\000\
\000\000\012\000\000\000\000\000\000\000\000\000\000\000\000\000\
\015\000\015\000\015\000\015\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\015\000\015\000\
\000\000\000\000\000\000\015\000\000\000\015\000\000\000\000\000\
\000\000\015\000\000\000\000\000\000\000\000\000\000\000\000\000\
\015\000\000\000\000\000\000\000\015\000\015\000\011\000\000\000\
\000\000\000\000\015\000\032\000\033\000\034\000\035\000\000\000\
\036\000\037\000\000\000\000\000\000\000\000\000\000\000\038\000\
\039\000\040\000\041\000\042\000\043\000\044\000\045\000\034\000\
\035\000\000\000\000\000\037\000\000\000\000\000\000\000\000\000\
\000\000\038\000\039\000\040\000\041\000\042\000\043\000\044\000\
\045\000\037\000\000\000\000\000\000\000\000\000\000\000\038\000\
\039\000\040\000\041\000\042\000\043\000\044\000\045\000\038\000\
\039\000\040\000\041\000\042\000\043\000\044\000\045\000"

let yycheck = "\013\000\
\000\000\000\000\049\001\017\000\008\001\009\001\010\001\011\001\
\005\001\001\001\002\001\003\001\004\001\001\000\006\001\007\001\
\006\001\010\001\022\001\023\001\028\001\013\001\014\001\027\001\
\049\000\029\001\051\000\019\001\020\001\033\001\001\001\002\001\
\003\001\004\001\040\001\006\001\040\001\042\001\035\001\040\001\
\044\001\025\001\001\001\002\001\003\001\004\001\050\001\006\001\
\007\001\041\001\035\001\041\001\038\001\039\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\001\001\002\001\
\003\001\004\001\040\001\006\001\007\001\010\001\041\001\001\001\
\002\001\003\001\004\001\036\001\037\001\007\001\024\001\093\000\
\094\000\045\001\041\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\001\001\002\001\003\001\004\001\109\000\
\040\001\007\001\015\001\016\001\017\001\018\001\041\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\041\001\
\041\001\042\001\001\001\002\001\003\001\004\001\010\001\006\001\
\001\001\002\001\003\001\004\001\038\001\039\001\007\001\040\001\
\046\001\041\001\010\001\041\001\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\001\001\002\001\003\001\004\001\
\043\001\006\001\007\001\024\001\024\001\034\001\001\001\002\001\
\013\001\014\001\041\001\006\001\001\001\002\001\019\001\020\001\
\041\001\006\001\034\001\001\001\002\001\003\001\004\001\010\001\
\006\001\007\001\001\001\002\001\003\001\004\001\044\001\006\001\
\007\001\044\001\010\001\034\001\041\001\019\001\020\001\010\001\
\010\001\034\001\045\001\045\001\019\001\020\001\041\001\012\000\
\002\001\041\001\051\001\034\001\041\001\044\001\019\000\010\001\
\034\001\045\001\008\001\041\001\046\001\026\000\041\001\000\000\
\029\000\045\001\041\001\032\000\033\000\034\000\035\000\043\001\
\037\000\038\000\039\000\040\000\041\000\042\000\043\000\044\000\
\045\000\008\001\009\001\010\001\011\001\013\001\014\001\015\001\
\016\001\017\001\018\001\001\001\002\001\003\001\004\001\022\001\
\023\001\007\001\043\001\082\000\027\001\255\255\029\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\255\255\
\255\255\040\001\255\255\255\255\255\255\255\255\255\255\255\255\
\008\001\009\001\010\001\011\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\022\001\023\001\
\255\255\255\255\255\255\027\001\255\255\029\001\255\255\255\255\
\255\255\033\001\255\255\255\255\255\255\255\255\255\255\255\255\
\040\001\255\255\255\255\255\255\044\001\045\001\045\001\255\255\
\255\255\255\255\050\001\001\001\002\001\003\001\004\001\255\255\
\006\001\007\001\255\255\255\255\255\255\255\255\255\255\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\003\001\
\004\001\255\255\255\255\007\001\255\255\255\255\255\255\255\255\
\255\255\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\007\001\255\255\255\255\255\255\255\255\255\255\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001"

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
  INTO\000\
  DELETE\000\
  UNION\000\
  INTERSECT\000\
  APPLY\000\
  WHILE\000\
  QUOTES\000\
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
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 39 "parser.mly"
                                          ( Vertex(_4) )
# 383 "parser.ml"
               : 'graph_element))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _11 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 40 "parser.mly"
                                                                                   ( Edge(_4, _8, _11) )
# 392 "parser.ml"
               : 'graph_element))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'graph_element) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'graph_elements) in
    Obj.repr(
# 43 "parser.mly"
                                         ( _1::_3 )
# 400 "parser.ml"
               : 'graph_elements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'graph_element) in
    Obj.repr(
# 44 "parser.mly"
                    ( [_1] )
# 407 "parser.ml"
               : 'graph_elements))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "parser.mly"
                ( [] )
# 413 "parser.ml"
               : 'graph_elements))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'graph_elements) in
    Obj.repr(
# 49 "parser.mly"
                           (_2)
# 420 "parser.ml"
               : 'graph_elements_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
                         ( Graph([]) )
# 426 "parser.ml"
               : 'graph_operation))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'graph_elements_list) in
    Obj.repr(
# 54 "parser.mly"
                                             ( Graph(_4) )
# 433 "parser.ml"
               : 'graph_operation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "parser.mly"
                                                 ( GraphAccess(_6, "vertices") )
# 441 "parser.ml"
               : 'graph_operation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "parser.mly"
                                              ( GraphAccess(_6, "edges") )
# 449 "parser.ml"
               : 'graph_operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
                  ( [] )
# 455 "parser.ml"
               : Ast.stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt_list) in
    Obj.repr(
# 60 "parser.mly"
                     (print_endline("Processing all stmts"); _1::_2 )
# 463 "parser.ml"
               : Ast.stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 63 "parser.mly"
                     ( Expr(_1) )
# 470 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt_list) in
    Obj.repr(
# 64 "parser.mly"
                      ( Block(_2) )
# 477 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt_list) in
    Obj.repr(
# 65 "parser.mly"
                                    ( print_endline("Parser If"); If(_3, _6) )
# 485 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : Ast.stmt_list) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt_list) in
    Obj.repr(
# 66 "parser.mly"
                                                         ( IfElse(_3, _6, _10))
# 494 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt_list) in
    Obj.repr(
# 67 "parser.mly"
                                       ( While(_3, _6))
# 502 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 70 "parser.mly"
                 ( Lit(_1) )
# 509 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 71 "parser.mly"
               ( FloatLit(_1) )
# 516 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 72 "parser.mly"
               ( BoolLit(_1) )
# 523 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
                           (Asn(_1, _3))
# 531 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 74 "parser.mly"
               ( Var(_1) )
# 538 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 75 "parser.mly"
                            ( GraphAccess(_1, "vertices") )
# 545 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 76 "parser.mly"
                         ( GraphAccess(_1, "edges") )
# 552 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'graph_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 77 "parser.mly"
                                  (GraphAsn(_3, _1))
# 560 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'graph_elements_list) in
    Obj.repr(
# 78 "parser.mly"
                                               (GraphOp(_3, _4, "insert"))
# 568 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'graph_elements_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 79 "parser.mly"
                                               (GraphOp(_4, _2, "delete"))
# 576 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 80 "parser.mly"
                     (Binop(_1, Add, _3) )
# 584 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "parser.mly"
                      ( Binop(_1, Sub, _3) )
# 592 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 82 "parser.mly"
                      ( Binop(_1, Mul, _3) )
# 600 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 83 "parser.mly"
                       ( Binop(_1, Div, _3) )
# 608 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "parser.mly"
                        ( Binop(_1, Mod, _3) )
# 616 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 85 "parser.mly"
                    ( Binop(_1, Eq, _3) )
# 624 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 86 "parser.mly"
                       ( Binop(_1, Neq, _3) )
# 632 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 87 "parser.mly"
                   ( Binop(_1, Gt, _3) )
# 640 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 88 "parser.mly"
                   ( Binop(_1, Lt, _3) )
# 648 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 89 "parser.mly"
                     ( Binop(_1, Gteq, _3) )
# 656 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 90 "parser.mly"
                     ( Binop(_1, Lteq, _3) )
# 664 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "parser.mly"
                    ( Binop(_1, And, _3) )
# 672 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "parser.mly"
                   ( Binop(_1, Or, _3) )
# 680 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 93 "parser.mly"
                 ( _2 )
# 687 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 97 "parser.mly"
           ( _1 )
# 694 "parser.ml"
               : 'entry))
(* Entry stmt_list *)
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
let stmt_list (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.stmt_list)
