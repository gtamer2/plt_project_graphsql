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
  | UPDATE
  | APPLY
  | WHILE
  | FOR
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
# 85 "parser.ml"
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
  288 (* UPDATE *);
  289 (* APPLY *);
  290 (* WHILE *);
  291 (* FOR *);
  292 (* QUOTES *);
  293 (* DOT *);
  294 (* VERTEX *);
  295 (* EDGE *);
  296 (* VERTICES *);
  297 (* EDGES *);
  298 (* LP *);
  299 (* RP *);
  300 (* LB *);
  301 (* RB *);
  302 (* LC *);
  303 (* RC *);
  304 (* COMMA *);
  305 (* ARROW *);
  306 (* COMMENT *);
  307 (* GRAPH *);
  308 (* IF *);
  309 (* ELSE *);
  310 (* ELIF *);
  311 (* DEFINE *);
  312 (* FUNCTION *);
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
\005\000\005\000\005\000\005\000\001\000\001\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\008\000\008\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\009\000\000\000"

let yylen = "\002\000\
\006\000\012\000\003\000\001\000\000\000\003\000\004\000\005\000\
\006\000\006\000\005\000\005\000\000\000\002\000\002\000\003\000\
\007\000\012\000\011\000\007\000\011\000\007\000\008\000\001\000\
\001\000\001\000\003\000\001\000\003\000\003\000\003\000\004\000\
\004\000\004\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\024\000\026\000\000\000\025\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\050\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\014\000\000\000\000\000\
\000\000\000\000\015\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\029\000\030\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\048\000\016\000\000\000\031\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\042\000\
\043\000\044\000\045\000\000\000\000\000\007\000\000\000\000\000\
\000\000\032\000\000\000\006\000\033\000\000\000\000\000\034\000\
\000\000\000\000\000\000\000\000\000\000\008\000\000\000\000\000\
\003\000\000\000\000\000\000\000\000\000\011\000\012\000\000\000\
\009\000\010\000\000\000\000\000\000\000\000\000\000\000\001\000\
\000\000\020\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\021\000\019\000\000\000\000\000\
\000\000\000\000\018\000\002\000\000\000\000\000\023\000"

let yydgoto = "\002\000\
\017\000\059\000\060\000\027\000\018\000\019\000\020\000\129\000\
\000\000"

let yysindex = "\008\000\
\034\255\000\000\000\000\000\000\065\255\000\000\220\254\022\255\
\026\255\006\255\244\254\020\255\025\255\100\000\034\255\029\255\
\000\000\054\255\034\255\157\000\125\000\041\255\047\255\046\255\
\086\255\244\254\074\255\066\255\067\255\094\255\125\000\125\000\
\172\255\146\255\072\255\125\000\110\255\000\000\125\000\125\000\
\125\000\125\000\000\000\125\000\125\000\125\000\125\000\125\000\
\125\000\125\000\125\000\125\000\217\000\000\000\000\000\057\255\
\063\255\006\255\073\255\078\255\115\255\090\255\098\255\125\255\
\177\255\177\000\127\255\128\255\000\000\000\000\197\255\000\000\
\235\000\235\000\249\000\249\000\001\001\246\254\246\254\000\000\
\000\000\000\000\000\000\157\255\157\255\000\000\108\255\130\255\
\131\255\000\000\244\254\000\000\000\000\147\255\148\255\000\000\
\121\255\125\000\113\255\133\255\122\255\000\000\173\255\175\255\
\000\000\150\255\151\255\034\255\197\000\000\000\000\000\034\255\
\000\000\000\000\139\255\186\255\158\255\125\000\159\255\000\000\
\171\255\000\000\228\255\052\255\198\255\179\255\181\255\176\255\
\166\255\192\255\034\255\034\255\125\000\187\255\188\255\190\255\
\191\255\053\000\034\255\226\255\000\000\000\000\193\255\202\255\
\207\255\034\255\000\000\000\000\208\255\204\255\000\000"

let yyrindex = "\000\000\
\003\001\000\000\000\000\000\000\126\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\213\255\000\000\
\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\
\000\000\216\255\000\000\000\000\000\000\000\000\000\000\000\000\
\073\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\049\255\000\000\000\000\000\000\
\000\000\000\000\217\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\017\255\093\255\109\255\220\255\010\255\045\255\071\255\000\000\
\000\000\000\000\000\000\250\255\098\000\000\000\000\000\000\000\
\000\000\000\000\216\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\213\255\000\000\000\000\000\000\213\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\213\255\213\255\000\000\000\000\000\000\000\000\
\000\000\000\000\213\255\000\000\000\000\000\000\000\000\000\000\
\000\000\213\255\000\000\000\000\000\000\211\255\000\000"

let yygindex = "\000\000\
\241\255\005\001\182\000\222\255\000\000\000\000\245\255\124\000\
\000\000"

let yytablesize = 533
let yytable = "\035\000\
\017\000\013\000\034\000\038\000\047\000\048\000\049\000\050\000\
\001\000\053\000\039\000\039\000\039\000\039\000\023\000\039\000\
\039\000\035\000\035\000\065\000\066\000\087\000\035\000\090\000\
\071\000\028\000\029\000\073\000\074\000\075\000\076\000\024\000\
\077\000\078\000\079\000\080\000\081\000\082\000\083\000\084\000\
\085\000\003\000\004\000\005\000\006\000\040\000\040\000\040\000\
\040\000\026\000\040\000\040\000\039\000\025\000\027\000\007\000\
\008\000\040\000\040\000\035\000\009\000\031\000\010\000\040\000\
\040\000\011\000\032\000\012\000\013\000\021\000\036\000\041\000\
\041\000\041\000\041\000\014\000\041\000\041\000\037\000\015\000\
\054\000\055\000\057\000\041\000\041\000\016\000\109\000\040\000\
\056\000\041\000\041\000\027\000\117\000\036\000\036\000\058\000\
\119\000\061\000\036\000\086\000\026\000\022\000\088\000\089\000\
\127\000\128\000\123\000\062\000\063\000\037\000\037\000\037\000\
\037\000\041\000\037\000\136\000\137\000\064\000\070\000\072\000\
\091\000\138\000\092\000\144\000\093\000\094\000\028\000\028\000\
\028\000\028\000\149\000\028\000\028\000\095\000\096\000\036\000\
\099\000\100\000\028\000\028\000\028\000\028\000\028\000\028\000\
\028\000\028\000\039\000\040\000\041\000\042\000\102\000\037\000\
\044\000\103\000\104\000\110\000\106\000\107\000\045\000\046\000\
\047\000\048\000\049\000\050\000\051\000\052\000\108\000\112\000\
\028\000\045\000\046\000\047\000\048\000\049\000\050\000\111\000\
\021\000\039\000\040\000\041\000\042\000\120\000\113\000\044\000\
\114\000\115\000\116\000\121\000\069\000\045\000\046\000\047\000\
\048\000\049\000\050\000\051\000\052\000\039\000\040\000\041\000\
\042\000\067\000\068\000\044\000\122\000\124\000\125\000\130\000\
\022\000\045\000\046\000\047\000\048\000\049\000\050\000\051\000\
\052\000\133\000\134\000\097\000\038\000\038\000\038\000\038\000\
\131\000\038\000\132\000\135\000\039\000\040\000\041\000\042\000\
\139\000\145\000\044\000\140\000\141\000\142\000\146\000\101\000\
\045\000\046\000\047\000\048\000\049\000\050\000\051\000\052\000\
\147\000\148\000\046\000\046\000\046\000\046\000\150\000\046\000\
\046\000\128\000\013\000\013\000\005\000\004\000\038\000\022\000\
\017\000\017\000\017\000\017\000\046\000\046\000\126\000\030\000\
\105\000\151\000\000\000\000\000\000\000\000\000\017\000\017\000\
\000\000\000\000\000\000\017\000\000\000\017\000\000\000\000\000\
\017\000\000\000\017\000\017\000\046\000\000\000\000\000\000\000\
\000\000\000\000\017\000\000\000\000\000\000\000\017\000\017\000\
\013\000\000\000\000\000\000\000\017\000\039\000\040\000\041\000\
\042\000\000\000\000\000\044\000\000\000\000\000\000\000\000\000\
\000\000\045\000\046\000\047\000\048\000\049\000\050\000\051\000\
\052\000\028\000\028\000\028\000\028\000\000\000\000\000\028\000\
\000\000\000\000\000\000\000\000\000\000\028\000\028\000\028\000\
\028\000\028\000\028\000\028\000\028\000\000\000\000\000\143\000\
\000\000\000\000\047\000\047\000\047\000\047\000\000\000\047\000\
\047\000\000\000\000\000\003\000\004\000\033\000\006\000\000\000\
\000\000\000\000\000\000\028\000\047\000\047\000\000\000\000\000\
\000\000\007\000\008\000\000\000\000\000\000\000\009\000\000\000\
\010\000\000\000\000\000\011\000\003\000\004\000\005\000\006\000\
\000\000\000\000\000\000\000\000\047\000\014\000\000\000\000\000\
\000\000\000\000\007\000\008\000\000\000\000\000\000\000\009\000\
\000\000\010\000\000\000\000\000\011\000\039\000\040\000\041\000\
\042\000\000\000\043\000\044\000\000\000\000\000\014\000\000\000\
\000\000\045\000\046\000\047\000\048\000\049\000\050\000\051\000\
\052\000\039\000\040\000\041\000\042\000\000\000\098\000\044\000\
\000\000\000\000\000\000\000\000\000\000\045\000\046\000\047\000\
\048\000\049\000\050\000\051\000\052\000\039\000\040\000\041\000\
\042\000\000\000\118\000\044\000\000\000\000\000\000\000\000\000\
\000\000\045\000\046\000\047\000\048\000\049\000\050\000\051\000\
\052\000\039\000\040\000\041\000\042\000\000\000\000\000\044\000\
\000\000\000\000\000\000\000\000\000\000\045\000\046\000\047\000\
\048\000\049\000\050\000\051\000\052\000\041\000\042\000\000\000\
\000\000\044\000\000\000\000\000\000\000\000\000\000\000\045\000\
\046\000\047\000\048\000\049\000\050\000\051\000\052\000\044\000\
\000\000\000\000\000\000\000\000\000\000\045\000\046\000\047\000\
\048\000\049\000\050\000\051\000\052\000\045\000\046\000\047\000\
\048\000\049\000\050\000\051\000\052\000"

let yycheck = "\015\000\
\000\000\000\000\014\000\019\000\015\001\016\001\017\001\018\001\
\001\000\021\000\001\001\002\001\003\001\004\001\051\001\006\001\
\007\001\001\001\002\001\031\000\032\000\056\000\006\001\058\000\
\036\000\038\001\039\001\039\000\040\000\041\000\042\000\010\001\
\044\000\045\000\046\000\047\000\048\000\049\000\050\000\051\000\
\052\000\008\001\009\001\010\001\011\001\001\001\002\001\003\001\
\004\001\044\001\006\001\007\001\043\001\028\001\006\001\022\001\
\023\001\013\001\014\001\043\001\027\001\042\001\029\001\019\001\
\020\001\032\001\042\001\034\001\035\001\005\001\042\001\001\001\
\002\001\003\001\004\001\042\001\006\001\007\001\025\001\046\001\
\040\001\041\001\037\001\013\001\014\001\052\001\098\000\043\001\
\042\001\019\001\020\001\043\001\108\000\001\001\002\001\010\001\
\112\000\024\001\006\001\043\001\044\001\037\001\040\001\041\001\
\053\001\054\001\118\000\042\001\042\001\001\001\002\001\003\001\
\004\001\043\001\006\001\131\000\132\000\024\001\047\001\010\001\
\048\001\133\000\045\001\139\000\010\001\036\001\001\001\002\001\
\003\001\004\001\146\000\006\001\007\001\036\001\010\001\043\001\
\010\001\010\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\001\001\002\001\003\001\004\001\043\001\043\001\
\007\001\024\001\024\001\043\001\010\001\010\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\046\001\046\001\
\043\001\013\001\014\001\015\001\016\001\017\001\018\001\043\001\
\005\001\001\001\002\001\003\001\004\001\043\001\010\001\007\001\
\010\001\036\001\036\001\002\001\043\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\001\001\002\001\003\001\
\004\001\030\001\031\001\007\001\047\001\047\001\036\001\010\001\
\037\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\042\001\053\001\043\001\001\001\002\001\003\001\004\001\
\046\001\006\001\046\001\036\001\001\001\002\001\003\001\004\001\
\046\001\008\001\007\001\048\001\047\001\047\001\046\001\043\001\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\047\001\043\001\001\001\002\001\003\001\004\001\047\001\006\001\
\007\001\054\001\000\000\047\001\045\001\045\001\043\001\053\001\
\008\001\009\001\010\001\011\001\019\001\020\001\043\001\011\000\
\091\000\150\000\255\255\255\255\255\255\255\255\022\001\023\001\
\255\255\255\255\255\255\027\001\255\255\029\001\255\255\255\255\
\032\001\255\255\034\001\035\001\043\001\255\255\255\255\255\255\
\255\255\255\255\042\001\255\255\255\255\255\255\046\001\047\001\
\047\001\255\255\255\255\255\255\052\001\001\001\002\001\003\001\
\004\001\255\255\255\255\007\001\255\255\255\255\255\255\255\255\
\255\255\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\001\001\002\001\003\001\004\001\255\255\255\255\007\001\
\255\255\255\255\255\255\255\255\255\255\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\255\255\255\255\043\001\
\255\255\255\255\001\001\002\001\003\001\004\001\255\255\006\001\
\007\001\255\255\255\255\008\001\009\001\010\001\011\001\255\255\
\255\255\255\255\255\255\043\001\019\001\020\001\255\255\255\255\
\255\255\022\001\023\001\255\255\255\255\255\255\027\001\255\255\
\029\001\255\255\255\255\032\001\008\001\009\001\010\001\011\001\
\255\255\255\255\255\255\255\255\043\001\042\001\255\255\255\255\
\255\255\255\255\022\001\023\001\255\255\255\255\255\255\027\001\
\255\255\029\001\255\255\255\255\032\001\001\001\002\001\003\001\
\004\001\255\255\006\001\007\001\255\255\255\255\042\001\255\255\
\255\255\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\001\001\002\001\003\001\004\001\255\255\006\001\007\001\
\255\255\255\255\255\255\255\255\255\255\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\001\001\002\001\003\001\
\004\001\255\255\006\001\007\001\255\255\255\255\255\255\255\255\
\255\255\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\001\001\002\001\003\001\004\001\255\255\255\255\007\001\
\255\255\255\255\255\255\255\255\255\255\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\003\001\004\001\255\255\
\255\255\007\001\255\255\255\255\255\255\255\255\255\255\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\007\001\
\255\255\255\255\255\255\255\255\255\255\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001"

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
  UPDATE\000\
  APPLY\000\
  WHILE\000\
  FOR\000\
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
# 447 "parser.ml"
               : 'graph_element))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _11 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 40 "parser.mly"
                                                                                   ( Edge(_4, _8, _11) )
# 456 "parser.ml"
               : 'graph_element))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'graph_element) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'graph_elements) in
    Obj.repr(
# 43 "parser.mly"
                                         ( _1::_3 )
# 464 "parser.ml"
               : 'graph_elements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'graph_element) in
    Obj.repr(
# 44 "parser.mly"
                    ( [_1] )
# 471 "parser.ml"
               : 'graph_elements))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "parser.mly"
                ( [] )
# 477 "parser.ml"
               : 'graph_elements))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'graph_elements) in
    Obj.repr(
# 49 "parser.mly"
                           (_2)
# 484 "parser.ml"
               : 'graph_elements_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
                         ( Graph([]) )
# 490 "parser.ml"
               : 'graph_operation))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'graph_elements_list) in
    Obj.repr(
# 54 "parser.mly"
                                             ( Graph(_4) )
# 497 "parser.ml"
               : 'graph_operation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "parser.mly"
                                                 ( GraphAccess(_6, "vertices") )
# 505 "parser.ml"
               : 'graph_operation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "parser.mly"
                                              ( GraphAccess(_6, "edges") )
# 513 "parser.ml"
               : 'graph_operation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 57 "parser.mly"
                                    ( GraphQuery(_2, _4, "union") )
# 521 "parser.ml"
               : 'graph_operation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 58 "parser.mly"
                                        ( GraphQuery(_2, _4, "intersect") )
# 529 "parser.ml"
               : 'graph_operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
                  ( [] )
# 535 "parser.ml"
               : Ast.stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt_list) in
    Obj.repr(
# 62 "parser.mly"
                     (print_endline("Processing all stmts"); _1::_2 )
# 543 "parser.ml"
               : Ast.stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 65 "parser.mly"
                     ( Expr(_1) )
# 550 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt_list) in
    Obj.repr(
# 66 "parser.mly"
                      ( Block(_2) )
# 557 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt_list) in
    Obj.repr(
# 67 "parser.mly"
                                    ( If(_3, _6) )
# 565 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 9 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 6 : Ast.stmt_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 4 : 'elif_stmt_list) in
    let _11 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt_list) in
    Obj.repr(
# 68 "parser.mly"
                                                                       ( IfElif(_3, _6, _8, _11))
# 575 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : Ast.stmt_list) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt_list) in
    Obj.repr(
# 69 "parser.mly"
                                                         ( IfElse(_3, _6, _10))
# 584 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt_list) in
    Obj.repr(
# 70 "parser.mly"
                                       ( While(_3, _6))
# 592 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt_list) in
    Obj.repr(
# 71 "parser.mly"
                                                                   ( For(_3, _5, _7, _10))
# 602 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt_list) in
    Obj.repr(
# 74 "parser.mly"
                                       ([(_3, _6)])
# 610 "parser.ml"
               : 'elif_stmt_list))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.stmt_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'elif_stmt_list) in
    Obj.repr(
# 75 "parser.mly"
                                                     ((_3, _6)::_8)
# 619 "parser.ml"
               : 'elif_stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 78 "parser.mly"
                 ( Lit(_1) )
# 626 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 79 "parser.mly"
               ( FloatLit(_1) )
# 633 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 80 "parser.mly"
               ( BoolLit(_1) )
# 640 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "parser.mly"
                           (Asn(_1, _3))
# 648 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 82 "parser.mly"
               ( Var(_1) )
# 655 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 83 "parser.mly"
                            ( GraphAccess(_1, "vertices") )
# 662 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 84 "parser.mly"
                         ( GraphAccess(_1, "edges") )
# 669 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'graph_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "parser.mly"
                                  (GraphAsn(_3, _1))
# 677 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'graph_elements_list) in
    Obj.repr(
# 86 "parser.mly"
                                               (GraphOp(_3, _4, "insert"))
# 685 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'graph_elements_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 87 "parser.mly"
                                               (GraphOp(_4, _2, "delete"))
# 693 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'graph_element) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 88 "parser.mly"
                                         ( GraphUpdate(_4, _2) )
# 701 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 89 "parser.mly"
                     (Binop(_1, Add, _3) )
# 709 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 90 "parser.mly"
                      ( Binop(_1, Sub, _3) )
# 717 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "parser.mly"
                      ( Binop(_1, Mul, _3) )
# 725 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "parser.mly"
                       ( Binop(_1, Div, _3) )
# 733 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "parser.mly"
                        ( Binop(_1, Mod, _3) )
# 741 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "parser.mly"
                    ( Binop(_1, Eq, _3) )
# 749 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "parser.mly"
                       ( Binop(_1, Neq, _3) )
# 757 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "parser.mly"
                   ( Binop(_1, Gt, _3) )
# 765 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "parser.mly"
                   ( Binop(_1, Lt, _3) )
# 773 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
                     ( Binop(_1, Gteq, _3) )
# 781 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
                     ( Binop(_1, Lteq, _3) )
# 789 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
                    ( Binop(_1, And, _3) )
# 797 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                   ( Binop(_1, Or, _3) )
# 805 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                 ( _2 )
# 812 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
           ( _1 )
# 819 "parser.ml"
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
