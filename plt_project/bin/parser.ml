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
\005\000\005\000\005\000\005\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\006\000\000\000"

let yylen = "\002\000\
\006\000\012\000\003\000\001\000\000\000\003\000\004\000\005\000\
\006\000\006\000\005\000\005\000\001\000\001\000\001\000\003\000\
\001\000\003\000\003\000\003\000\004\000\004\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000\007\000\011\000\
\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\013\000\015\000\000\000\014\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\018\000\019\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\036\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\030\000\031\000\
\032\000\033\000\000\000\000\000\020\000\007\000\000\000\000\000\
\000\000\021\000\000\000\000\000\000\000\006\000\022\000\000\000\
\000\000\000\000\008\000\000\000\000\000\000\000\000\000\003\000\
\011\000\012\000\000\000\009\000\010\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\040\000\000\000\000\000\002\000"

let yydgoto = "\002\000\
\013\000\048\000\049\000\021\000\014\000\000\000"

let yysindex = "\255\255\
\082\255\000\000\000\000\000\000\005\255\000\000\209\254\010\255\
\239\254\025\255\104\255\043\255\233\255\022\255\082\255\012\255\
\046\255\071\255\098\255\016\255\086\255\007\255\164\255\082\255\
\082\255\082\255\082\255\082\255\082\255\082\255\082\255\082\255\
\082\255\082\255\082\255\082\255\082\255\082\255\106\255\210\000\
\000\000\000\000\029\255\034\255\025\255\077\255\078\255\073\255\
\085\255\110\255\111\255\114\255\000\000\184\255\081\255\081\255\
\224\000\224\000\210\000\193\255\246\254\246\254\000\000\000\000\
\000\000\000\000\001\255\001\255\000\000\000\000\084\255\105\255\
\112\255\000\000\100\255\101\255\016\255\000\000\000\000\096\255\
\097\255\099\255\000\000\130\255\131\255\132\255\135\255\000\000\
\000\000\000\000\082\255\000\000\000\000\113\255\115\255\042\255\
\107\255\144\255\102\255\000\000\117\255\108\255\145\255\082\255\
\122\255\062\255\116\255\000\000\150\255\118\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\160\000\000\000\000\000\000\000\
\000\000\000\000\000\000\120\255\000\000\213\255\000\000\000\000\
\000\000\000\000\000\000\000\000\021\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\126\255\
\000\000\000\000\000\000\000\000\000\000\000\000\139\000\164\000\
\157\000\175\000\004\000\150\000\074\000\103\000\000\000\000\000\
\000\000\000\000\123\000\130\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\120\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\054\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\254\255\000\000\084\000\252\255\000\000\000\000"

let yytablesize = 500
let yytable = "\001\000\
\017\000\017\000\016\000\037\000\033\000\034\000\035\000\036\000\
\023\000\015\000\019\000\015\000\040\000\031\000\032\000\033\000\
\034\000\035\000\036\000\018\000\038\000\054\000\055\000\056\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\051\000\052\000\071\000\016\000\
\074\000\016\000\025\000\026\000\027\000\028\000\039\000\029\000\
\030\000\041\000\042\000\046\000\047\000\039\000\031\000\032\000\
\033\000\034\000\035\000\036\000\037\000\038\000\025\000\026\000\
\027\000\028\000\020\000\029\000\030\000\070\000\020\000\072\000\
\073\000\028\000\031\000\032\000\033\000\034\000\035\000\036\000\
\037\000\038\000\024\000\027\000\028\000\043\000\099\000\030\000\
\096\000\003\000\004\000\005\000\006\000\031\000\032\000\033\000\
\034\000\035\000\036\000\037\000\038\000\106\000\029\000\007\000\
\008\000\044\000\108\000\045\000\009\000\050\000\010\000\003\000\
\004\000\022\000\006\000\069\000\075\000\076\000\077\000\079\000\
\080\000\011\000\034\000\081\000\083\000\007\000\008\000\078\000\
\084\000\035\000\009\000\012\000\010\000\086\000\087\000\085\000\
\089\000\090\000\023\000\092\000\093\000\094\000\091\000\011\000\
\095\000\101\000\097\000\100\000\098\000\027\000\103\000\104\000\
\102\000\012\000\105\000\107\000\025\000\110\000\111\000\042\000\
\088\000\109\000\005\000\024\000\025\000\026\000\027\000\028\000\
\004\000\029\000\030\000\000\000\000\000\000\000\026\000\000\000\
\031\000\032\000\033\000\034\000\035\000\036\000\037\000\038\000\
\025\000\026\000\027\000\028\000\000\000\029\000\030\000\000\000\
\000\000\000\000\000\000\000\000\031\000\032\000\033\000\034\000\
\035\000\036\000\037\000\038\000\053\000\031\000\032\000\033\000\
\034\000\035\000\036\000\037\000\038\000\017\000\017\000\017\000\
\017\000\000\000\017\000\017\000\000\000\000\000\000\000\000\000\
\082\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
\017\000\025\000\026\000\027\000\028\000\000\000\029\000\030\000\
\000\000\000\000\000\000\000\000\000\000\031\000\032\000\033\000\
\034\000\035\000\036\000\037\000\038\000\017\000\000\000\000\000\
\000\000\017\000\017\000\017\000\017\000\000\000\017\000\017\000\
\016\000\037\000\000\000\000\000\000\000\017\000\017\000\017\000\
\017\000\017\000\017\000\017\000\017\000\038\000\038\000\038\000\
\038\000\000\000\038\000\038\000\000\000\000\000\000\000\000\000\
\000\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
\038\000\017\000\000\000\016\000\037\000\017\000\000\000\016\000\
\037\000\000\000\000\000\000\000\000\000\000\000\039\000\039\000\
\039\000\039\000\000\000\039\000\039\000\038\000\000\000\000\000\
\000\000\038\000\039\000\039\000\039\000\039\000\039\000\039\000\
\039\000\039\000\028\000\028\000\028\000\028\000\000\000\028\000\
\028\000\000\000\000\000\000\000\000\000\000\000\028\000\028\000\
\000\000\000\000\000\000\000\000\028\000\028\000\039\000\000\000\
\000\000\000\000\039\000\000\000\000\000\000\000\000\000\029\000\
\029\000\029\000\029\000\000\000\029\000\029\000\000\000\000\000\
\000\000\000\000\028\000\029\000\029\000\000\000\028\000\000\000\
\000\000\029\000\029\000\034\000\034\000\034\000\034\000\000\000\
\034\000\034\000\035\000\035\000\035\000\035\000\000\000\035\000\
\035\000\000\000\000\000\023\000\023\000\034\000\034\000\029\000\
\023\000\000\000\000\000\029\000\035\000\035\000\027\000\027\000\
\027\000\027\000\000\000\027\000\027\000\025\000\025\000\025\000\
\025\000\000\000\025\000\034\000\024\000\024\000\000\000\034\000\
\000\000\024\000\035\000\000\000\000\000\000\000\035\000\026\000\
\026\000\026\000\026\000\023\000\026\000\000\000\000\000\023\000\
\000\000\000\000\000\000\000\000\000\000\000\000\027\000\000\000\
\000\000\000\000\027\000\000\000\000\000\025\000\000\000\000\000\
\000\000\025\000\000\000\000\000\024\000\000\000\000\000\000\000\
\024\000\000\000\025\000\026\000\027\000\028\000\000\000\026\000\
\030\000\000\000\000\000\026\000\000\000\000\000\031\000\032\000\
\033\000\034\000\035\000\036\000\037\000\038\000\030\000\000\000\
\000\000\000\000\000\000\000\000\031\000\032\000\033\000\034\000\
\035\000\036\000\037\000\038\000"

let yycheck = "\001\000\
\000\000\049\001\000\000\000\000\015\001\016\001\017\001\018\001\
\011\000\005\001\028\001\005\001\015\000\013\001\014\001\015\001\
\016\001\017\001\018\001\010\001\000\000\024\000\025\000\026\000\
\027\000\028\000\029\000\030\000\031\000\032\000\033\000\034\000\
\035\000\036\000\037\000\038\000\030\001\031\001\043\000\035\001\
\045\000\035\001\001\001\002\001\003\001\004\001\025\001\006\001\
\007\001\038\001\039\001\036\001\037\001\000\000\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\001\001\002\001\
\003\001\004\001\042\001\006\001\007\001\041\001\042\001\038\001\
\039\001\000\000\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\040\001\003\001\004\001\040\001\045\001\007\001\
\091\000\008\001\009\001\010\001\011\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\104\000\000\000\022\001\
\023\001\035\001\045\001\010\001\027\001\024\001\029\001\008\001\
\009\001\010\001\011\001\010\001\040\001\040\001\046\001\010\001\
\010\001\040\001\000\000\010\001\041\001\022\001\023\001\043\001\
\024\001\000\000\027\001\050\001\029\001\034\001\034\001\024\001\
\041\001\041\001\000\000\010\001\010\001\010\001\044\001\040\001\
\010\001\002\001\034\001\041\001\034\001\000\000\034\001\044\001\
\051\001\050\001\010\001\034\001\000\000\008\001\041\001\000\000\
\077\000\046\001\043\001\000\000\001\001\002\001\003\001\004\001\
\043\001\006\001\007\001\255\255\255\255\255\255\000\000\255\255\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\001\001\002\001\003\001\004\001\255\255\006\001\007\001\255\255\
\255\255\255\255\255\255\255\255\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\041\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\001\001\002\001\003\001\
\004\001\255\255\006\001\007\001\255\255\255\255\255\255\255\255\
\041\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\001\001\002\001\003\001\004\001\255\255\006\001\007\001\
\255\255\255\255\255\255\255\255\255\255\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\041\001\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\255\255\006\001\007\001\
\006\001\006\001\255\255\255\255\255\255\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\001\001\002\001\003\001\
\004\001\255\255\006\001\007\001\255\255\255\255\255\255\255\255\
\255\255\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\041\001\255\255\041\001\041\001\045\001\255\255\045\001\
\045\001\255\255\255\255\255\255\255\255\255\255\001\001\002\001\
\003\001\004\001\255\255\006\001\007\001\041\001\255\255\255\255\
\255\255\045\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\001\001\002\001\003\001\004\001\255\255\006\001\
\007\001\255\255\255\255\255\255\255\255\255\255\013\001\014\001\
\255\255\255\255\255\255\255\255\019\001\020\001\041\001\255\255\
\255\255\255\255\045\001\255\255\255\255\255\255\255\255\001\001\
\002\001\003\001\004\001\255\255\006\001\007\001\255\255\255\255\
\255\255\255\255\041\001\013\001\014\001\255\255\045\001\255\255\
\255\255\019\001\020\001\001\001\002\001\003\001\004\001\255\255\
\006\001\007\001\001\001\002\001\003\001\004\001\255\255\006\001\
\007\001\255\255\255\255\001\001\002\001\019\001\020\001\041\001\
\006\001\255\255\255\255\045\001\019\001\020\001\001\001\002\001\
\003\001\004\001\255\255\006\001\007\001\001\001\002\001\003\001\
\004\001\255\255\006\001\041\001\001\001\002\001\255\255\045\001\
\255\255\006\001\041\001\255\255\255\255\255\255\045\001\001\001\
\002\001\003\001\004\001\041\001\006\001\255\255\255\255\045\001\
\255\255\255\255\255\255\255\255\255\255\255\255\041\001\255\255\
\255\255\255\255\045\001\255\255\255\255\041\001\255\255\255\255\
\255\255\045\001\255\255\255\255\041\001\255\255\255\255\255\255\
\045\001\255\255\001\001\002\001\003\001\004\001\255\255\041\001\
\007\001\255\255\255\255\045\001\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\007\001\255\255\
\255\255\255\255\255\255\255\255\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001"

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
# 41 "parser.mly"
                                          ( Vertex(_4) )
# 414 "parser.ml"
               : 'graph_element))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _11 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 42 "parser.mly"
                                                                                   ( Edge(_4, _8, _11) )
# 423 "parser.ml"
               : 'graph_element))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'graph_element) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'graph_elements) in
    Obj.repr(
# 45 "parser.mly"
                                         ( _1::_3 )
# 431 "parser.ml"
               : 'graph_elements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'graph_element) in
    Obj.repr(
# 46 "parser.mly"
                    ( [_1] )
# 438 "parser.ml"
               : 'graph_elements))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
                ( [] )
# 444 "parser.ml"
               : 'graph_elements))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'graph_elements) in
    Obj.repr(
# 51 "parser.mly"
                           (_2)
# 451 "parser.ml"
               : 'graph_elements_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
                         ( Graph([]) )
# 457 "parser.ml"
               : 'graph_operation))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'graph_elements_list) in
    Obj.repr(
# 56 "parser.mly"
                                             ( Graph(_4) )
# 464 "parser.ml"
               : 'graph_operation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "parser.mly"
                                                 ( GraphAccess(_6, "vertices") )
# 472 "parser.ml"
               : 'graph_operation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "parser.mly"
                                              ( GraphAccess(_6, "edges") )
# 480 "parser.ml"
               : 'graph_operation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 59 "parser.mly"
                                    ( GraphQuery(_2, _4, "union") )
# 488 "parser.ml"
               : 'graph_operation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 60 "parser.mly"
                                        ( GraphQuery(_2, _4, "intersect") )
# 496 "parser.ml"
               : 'graph_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 64 "parser.mly"
                 ( Lit(_1) )
# 503 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 65 "parser.mly"
               ( FloatLit(_1) )
# 510 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 66 "parser.mly"
               ( BoolLit(_1) )
# 517 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 67 "parser.mly"
                           (Asn(_1, _3))
# 525 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 68 "parser.mly"
               ( Var(_1) )
# 532 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 69 "parser.mly"
                            ( GraphAccess(_1, "vertices") )
# 539 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 70 "parser.mly"
                         ( GraphAccess(_1, "edges") )
# 546 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'graph_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 71 "parser.mly"
                                  (GraphAsn(_3, _1))
# 554 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'graph_elements_list) in
    Obj.repr(
# 72 "parser.mly"
                                               (GraphOp(_3, _4, "insert"))
# 562 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'graph_elements_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 73 "parser.mly"
                                               (GraphOp(_4, _2, "delete"))
# 570 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 74 "parser.mly"
                     (Binop(_1, Add, _3) )
# 578 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 75 "parser.mly"
                      ( Binop(_1, Sub, _3) )
# 586 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 76 "parser.mly"
                      ( Binop(_1, Mul, _3) )
# 594 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 77 "parser.mly"
                       ( Binop(_1, Div, _3) )
# 602 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 78 "parser.mly"
                        ( Binop(_1, Mod, _3) )
# 610 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 79 "parser.mly"
                    ( Binop(_1, Eq, _3) )
# 618 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 80 "parser.mly"
                       ( Binop(_1, Neq, _3) )
# 626 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 81 "parser.mly"
                   ( Binop(_1, Gt, _3) )
# 634 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 82 "parser.mly"
                   ( Binop(_1, Lt, _3) )
# 642 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 83 "parser.mly"
                     ( Binop(_1, Gteq, _3) )
# 650 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 84 "parser.mly"
                     ( Binop(_1, Lteq, _3) )
# 658 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 85 "parser.mly"
                    ( Binop(_1, And, _3) )
# 666 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 86 "parser.mly"
                   ( Binop(_1, Or, _3) )
# 674 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 87 "parser.mly"
                 ( _2 )
# 681 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 88 "parser.mly"
                          ( Seq(_1, _3) )
# 689 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 89 "parser.mly"
                     (_1)
# 696 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 90 "parser.mly"
                               ( print_endline "Parsing if"; If(_3, _6))
# 704 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : Ast.expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : Ast.expr) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 91 "parser.mly"
                                               ( print_endline "Parsing if/else"; IfElse(_3, _6, _10))
# 713 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 94 "parser.mly"
           ( _1 )
# 720 "parser.ml"
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
