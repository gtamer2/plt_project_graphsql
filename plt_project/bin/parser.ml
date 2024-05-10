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
  | RETURN
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
# 86 "parser.ml"
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
  292 (* RETURN *);
  293 (* QUOTES *);
  294 (* DOT *);
  295 (* VERTEX *);
  296 (* EDGE *);
  297 (* VERTICES *);
  298 (* EDGES *);
  299 (* LP *);
  300 (* RP *);
  301 (* LB *);
  302 (* RB *);
  303 (* LC *);
  304 (* RC *);
  305 (* COMMA *);
  306 (* ARROW *);
  307 (* COMMENT *);
  308 (* GRAPH *);
  309 (* IF *);
  310 (* ELSE *);
  311 (* ELIF *);
  312 (* DEFINE *);
  313 (* FUNCTION *);
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
\006\000\006\000\006\000\006\000\006\000\006\000\008\000\008\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\009\000\000\000"

let yylen = "\002\000\
\006\000\012\000\003\000\001\000\000\000\003\000\004\000\005\000\
\006\000\006\000\005\000\005\000\000\000\002\000\002\000\003\000\
\007\000\012\000\011\000\007\000\011\000\007\000\007\000\008\000\
\001\000\001\000\001\000\002\000\003\000\001\000\003\000\003\000\
\003\000\003\000\004\000\004\000\004\000\002\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\025\000\027\000\000\000\026\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\054\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\014\000\000\000\000\000\000\000\
\000\000\015\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\032\000\033\000\031\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\052\000\016\000\000\000\000\000\
\034\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\046\000\047\000\048\000\049\000\000\000\000\000\007\000\000\000\
\000\000\000\000\035\000\000\000\006\000\036\000\000\000\000\000\
\037\000\000\000\000\000\000\000\000\000\000\000\000\000\008\000\
\000\000\000\000\003\000\000\000\000\000\000\000\000\000\011\000\
\012\000\000\000\000\000\009\000\010\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\000\000\020\000\000\000\000\000\
\022\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\021\000\019\000\000\000\000\000\000\000\000\000\018\000\
\002\000\000\000\000\000\024\000"

let yydgoto = "\002\000\
\020\000\067\000\068\000\032\000\021\000\022\000\023\000\142\000\
\000\000"

let yysindex = "\006\000\
\050\255\000\000\000\000\000\000\025\255\000\000\134\000\207\254\
\008\255\250\254\251\254\227\254\019\255\024\255\134\000\170\000\
\050\255\031\255\054\255\000\000\053\255\050\255\208\000\134\000\
\246\254\037\255\104\255\040\255\060\255\089\255\227\254\078\255\
\065\255\066\255\086\255\134\000\134\000\012\001\219\255\163\255\
\067\255\134\000\082\255\121\255\000\000\134\000\134\000\134\000\
\134\000\000\000\134\000\134\000\134\000\134\000\134\000\134\000\
\134\000\134\000\134\000\012\001\000\000\000\000\000\000\249\254\
\028\255\251\254\084\255\090\255\124\255\100\255\102\255\130\255\
\195\255\228\000\131\255\138\255\000\000\000\000\215\255\107\255\
\000\000\169\000\169\000\026\001\026\001\175\255\104\255\104\255\
\000\000\000\000\000\000\000\000\090\000\090\000\000\000\108\255\
\129\255\144\255\000\000\227\254\000\000\000\000\145\255\159\255\
\000\000\125\255\134\000\127\255\140\255\126\255\128\255\000\000\
\164\255\176\255\000\000\148\255\166\255\050\255\248\000\000\000\
\000\000\050\255\050\255\000\000\000\000\156\255\199\255\157\255\
\134\000\158\255\172\255\000\000\167\255\000\000\058\000\033\255\
\000\000\211\255\178\255\179\255\180\255\173\255\200\255\050\255\
\050\255\134\000\189\255\191\255\190\255\193\255\078\000\050\255\
\238\255\000\000\000\000\204\255\205\255\208\255\050\255\000\000\
\000\000\210\255\201\255\000\000"

let yyrindex = "\000\000\
\007\001\000\000\000\000\000\000\143\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\216\255\000\000\000\000\000\000\000\000\002\000\000\000\000\000\
\000\000\000\000\022\255\000\000\000\000\000\000\227\255\000\000\
\000\000\000\000\000\000\000\000\000\000\003\255\110\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\021\255\000\000\000\000\000\000\000\000\
\000\000\000\000\230\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\074\255\094\255\088\255\039\000\013\255\110\255\241\255\
\000\000\000\000\000\000\000\000\012\000\130\000\000\000\000\000\
\000\000\000\000\000\000\227\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\216\255\000\000\000\000\
\000\000\216\255\216\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\216\255\
\216\255\000\000\000\000\000\000\000\000\000\000\000\000\216\255\
\000\000\000\000\000\000\000\000\000\000\000\000\216\255\000\000\
\000\000\000\000\223\255\000\000"

let yygindex = "\000\000\
\239\255\013\001\182\000\198\255\000\000\000\000\253\255\120\000\
\000\000"

let yytablesize = 558
let yytable = "\041\000\
\017\000\013\000\028\000\027\000\045\000\096\000\001\000\099\000\
\028\000\033\000\034\000\038\000\040\000\043\000\043\000\043\000\
\043\000\029\000\043\000\043\000\060\000\030\000\038\000\038\000\
\038\000\038\000\029\000\038\000\038\000\024\000\061\000\062\000\
\073\000\074\000\038\000\038\000\095\000\031\000\079\000\031\000\
\038\000\038\000\082\000\083\000\084\000\085\000\028\000\086\000\
\087\000\088\000\089\000\090\000\091\000\092\000\093\000\094\000\
\043\000\003\000\004\000\005\000\006\000\036\000\025\000\043\000\
\029\000\038\000\037\000\026\000\097\000\098\000\007\000\008\000\
\009\000\042\000\039\000\039\000\010\000\044\000\011\000\039\000\
\063\000\012\000\064\000\013\000\014\000\015\000\140\000\141\000\
\041\000\041\000\041\000\041\000\016\000\041\000\040\000\040\000\
\017\000\065\000\066\000\040\000\128\000\069\000\018\000\119\000\
\130\000\131\000\019\000\070\000\071\000\072\000\044\000\044\000\
\044\000\044\000\078\000\044\000\044\000\039\000\054\000\055\000\
\056\000\057\000\044\000\044\000\080\000\135\000\149\000\150\000\
\044\000\044\000\081\000\041\000\100\000\102\000\157\000\101\000\
\103\000\040\000\104\000\105\000\108\000\162\000\151\000\030\000\
\030\000\030\000\030\000\109\000\030\000\030\000\111\000\112\000\
\113\000\044\000\116\000\030\000\030\000\030\000\030\000\030\000\
\030\000\030\000\030\000\046\000\047\000\048\000\049\000\114\000\
\117\000\051\000\120\000\118\000\122\000\124\000\123\000\052\000\
\053\000\054\000\055\000\056\000\057\000\058\000\059\000\121\000\
\126\000\125\000\030\000\052\000\053\000\054\000\055\000\056\000\
\057\000\058\000\059\000\046\000\047\000\048\000\049\000\132\000\
\133\000\051\000\127\000\138\000\134\000\136\000\077\000\052\000\
\053\000\054\000\055\000\056\000\057\000\058\000\059\000\046\000\
\047\000\048\000\049\000\137\000\143\000\051\000\146\000\024\000\
\144\000\145\000\147\000\052\000\053\000\054\000\055\000\056\000\
\057\000\058\000\059\000\152\000\148\000\154\000\106\000\153\000\
\155\000\045\000\045\000\045\000\045\000\158\000\045\000\045\000\
\075\000\076\000\159\000\161\000\160\000\045\000\045\000\141\000\
\025\000\163\000\110\000\045\000\045\000\026\000\013\000\013\000\
\017\000\017\000\017\000\017\000\050\000\050\000\050\000\050\000\
\005\000\050\000\050\000\004\000\023\000\017\000\017\000\017\000\
\035\000\115\000\164\000\017\000\045\000\017\000\050\000\050\000\
\017\000\000\000\017\000\017\000\017\000\000\000\000\000\042\000\
\042\000\042\000\042\000\017\000\042\000\000\000\000\000\017\000\
\017\000\013\000\000\000\000\000\000\000\017\000\000\000\050\000\
\000\000\017\000\046\000\047\000\048\000\049\000\000\000\000\000\
\051\000\000\000\000\000\000\000\000\000\000\000\052\000\053\000\
\054\000\055\000\056\000\057\000\058\000\059\000\046\000\047\000\
\048\000\049\000\042\000\000\000\051\000\000\000\000\000\000\000\
\000\000\000\000\052\000\053\000\054\000\055\000\056\000\057\000\
\058\000\059\000\000\000\000\000\000\000\139\000\052\000\053\000\
\054\000\055\000\056\000\057\000\000\000\000\000\030\000\030\000\
\030\000\030\000\000\000\000\000\030\000\000\000\000\000\000\000\
\000\000\156\000\030\000\030\000\030\000\030\000\030\000\030\000\
\030\000\030\000\051\000\051\000\051\000\051\000\000\000\051\000\
\051\000\000\000\000\000\000\000\000\000\003\000\004\000\005\000\
\006\000\000\000\000\000\000\000\051\000\051\000\000\000\000\000\
\000\000\030\000\007\000\008\000\009\000\000\000\000\000\000\000\
\010\000\000\000\011\000\000\000\000\000\012\000\000\000\000\000\
\000\000\015\000\000\000\048\000\049\000\051\000\000\000\051\000\
\016\000\003\000\004\000\039\000\006\000\052\000\053\000\054\000\
\055\000\056\000\057\000\058\000\059\000\000\000\007\000\008\000\
\009\000\000\000\000\000\000\000\010\000\000\000\011\000\000\000\
\000\000\012\000\000\000\000\000\000\000\015\000\000\000\000\000\
\046\000\047\000\048\000\049\000\016\000\050\000\051\000\000\000\
\000\000\000\000\000\000\000\000\052\000\053\000\054\000\055\000\
\056\000\057\000\058\000\059\000\046\000\047\000\048\000\049\000\
\000\000\107\000\051\000\000\000\000\000\000\000\000\000\000\000\
\052\000\053\000\054\000\055\000\056\000\057\000\058\000\059\000\
\046\000\047\000\048\000\049\000\000\000\129\000\051\000\000\000\
\000\000\000\000\000\000\000\000\052\000\053\000\054\000\055\000\
\056\000\057\000\058\000\059\000\046\000\047\000\048\000\049\000\
\000\000\000\000\051\000\000\000\000\000\000\000\000\000\000\000\
\052\000\053\000\054\000\055\000\056\000\057\000\058\000\059\000\
\051\000\000\000\000\000\000\000\000\000\000\000\052\000\053\000\
\054\000\055\000\056\000\057\000\058\000\059\000"

let yycheck = "\017\000\
\000\000\000\000\052\001\007\000\022\000\064\000\001\000\066\000\
\006\001\039\001\040\001\015\000\016\000\001\001\002\001\003\001\
\004\001\010\001\006\001\007\001\024\000\028\001\001\001\002\001\
\003\001\004\001\006\001\006\001\007\001\005\001\041\001\042\001\
\036\000\037\000\013\001\014\001\044\001\045\001\042\000\045\001\
\019\001\020\001\046\000\047\000\048\000\049\000\044\001\051\000\
\052\000\053\000\054\000\055\000\056\000\057\000\058\000\059\000\
\044\001\008\001\009\001\010\001\011\001\043\001\038\001\010\001\
\044\001\044\001\043\001\043\001\041\001\042\001\021\001\022\001\
\023\001\043\001\001\001\002\001\027\001\025\001\029\001\006\001\
\044\001\032\001\043\001\034\001\035\001\036\001\054\001\055\001\
\001\001\002\001\003\001\004\001\043\001\006\001\001\001\002\001\
\047\001\038\001\010\001\006\001\118\000\024\001\053\001\107\000\
\122\000\123\000\057\001\043\001\043\001\024\001\001\001\002\001\
\003\001\004\001\048\001\006\001\007\001\044\001\015\001\016\001\
\017\001\018\001\013\001\014\001\043\001\129\000\144\000\145\000\
\019\001\020\001\010\001\044\001\049\001\010\001\152\000\046\001\
\037\001\044\001\037\001\010\001\010\001\159\000\146\000\001\001\
\002\001\003\001\004\001\010\001\006\001\007\001\044\001\044\001\
\024\001\044\001\010\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\001\001\002\001\003\001\004\001\024\001\
\010\001\007\001\044\001\047\001\047\001\010\001\047\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\044\001\
\037\001\010\001\044\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\001\001\002\001\003\001\004\001\044\001\
\002\001\007\001\037\001\037\001\048\001\048\001\044\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\001\001\
\002\001\003\001\004\001\048\001\010\001\007\001\043\001\005\001\
\047\001\047\001\054\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\047\001\037\001\048\001\044\001\049\001\
\048\001\001\001\002\001\003\001\004\001\008\001\006\001\007\001\
\030\001\031\001\047\001\044\001\048\001\013\001\014\001\055\001\
\038\001\048\001\044\001\019\001\020\001\043\001\000\000\048\001\
\008\001\009\001\010\001\011\001\001\001\002\001\003\001\004\001\
\046\001\006\001\007\001\046\001\054\001\021\001\022\001\023\001\
\012\000\100\000\163\000\027\001\044\001\029\001\019\001\020\001\
\032\001\255\255\034\001\035\001\036\001\255\255\255\255\001\001\
\002\001\003\001\004\001\043\001\006\001\255\255\255\255\047\001\
\048\001\048\001\255\255\255\255\255\255\053\001\255\255\044\001\
\255\255\057\001\001\001\002\001\003\001\004\001\255\255\255\255\
\007\001\255\255\255\255\255\255\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\001\001\002\001\
\003\001\004\001\044\001\255\255\007\001\255\255\255\255\255\255\
\255\255\255\255\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\255\255\255\255\255\255\044\001\013\001\014\001\
\015\001\016\001\017\001\018\001\255\255\255\255\001\001\002\001\
\003\001\004\001\255\255\255\255\007\001\255\255\255\255\255\255\
\255\255\044\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\001\001\002\001\003\001\004\001\255\255\006\001\
\007\001\255\255\255\255\255\255\255\255\008\001\009\001\010\001\
\011\001\255\255\255\255\255\255\019\001\020\001\255\255\255\255\
\255\255\044\001\021\001\022\001\023\001\255\255\255\255\255\255\
\027\001\255\255\029\001\255\255\255\255\032\001\255\255\255\255\
\255\255\036\001\255\255\003\001\004\001\044\001\255\255\007\001\
\043\001\008\001\009\001\010\001\011\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\255\255\021\001\022\001\
\023\001\255\255\255\255\255\255\027\001\255\255\029\001\255\255\
\255\255\032\001\255\255\255\255\255\255\036\001\255\255\255\255\
\001\001\002\001\003\001\004\001\043\001\006\001\007\001\255\255\
\255\255\255\255\255\255\255\255\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\001\001\002\001\003\001\004\001\
\255\255\006\001\007\001\255\255\255\255\255\255\255\255\255\255\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\001\001\002\001\003\001\004\001\255\255\006\001\007\001\255\255\
\255\255\255\255\255\255\255\255\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\001\001\002\001\003\001\004\001\
\255\255\255\255\007\001\255\255\255\255\255\255\255\255\255\255\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\007\001\255\255\255\255\255\255\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001"

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
  RETURN\000\
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
# 40 "parser.mly"
                                          ( Vertex(_4) )
# 462 "parser.ml"
               : 'graph_element))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _11 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 41 "parser.mly"
                                                                                   ( Edge(_4, _8, _11) )
# 471 "parser.ml"
               : 'graph_element))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'graph_element) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'graph_elements) in
    Obj.repr(
# 44 "parser.mly"
                                         ( _1::_3 )
# 479 "parser.ml"
               : 'graph_elements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'graph_element) in
    Obj.repr(
# 45 "parser.mly"
                    ( [_1] )
# 486 "parser.ml"
               : 'graph_elements))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
                ( [] )
# 492 "parser.ml"
               : 'graph_elements))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'graph_elements) in
    Obj.repr(
# 50 "parser.mly"
                           (_2)
# 499 "parser.ml"
               : 'graph_elements_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
                         ( Graph([]) )
# 505 "parser.ml"
               : 'graph_operation))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'graph_elements_list) in
    Obj.repr(
# 54 "parser.mly"
                                             ( Graph(_4) )
# 512 "parser.ml"
               : 'graph_operation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "parser.mly"
                                                 ( GraphAccess(_6, "vertices") )
# 520 "parser.ml"
               : 'graph_operation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "parser.mly"
                                              ( GraphAccess(_6, "edges") )
# 528 "parser.ml"
               : 'graph_operation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 57 "parser.mly"
                                    ( GraphQuery(_2, _4, "union") )
# 536 "parser.ml"
               : 'graph_operation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 58 "parser.mly"
                                        ( GraphQuery(_2, _4, "intersect") )
# 544 "parser.ml"
               : 'graph_operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
                  ( [] )
# 550 "parser.ml"
               : Ast.stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt_list) in
    Obj.repr(
# 62 "parser.mly"
                     (_1::_2 )
# 558 "parser.ml"
               : Ast.stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
                     ( Expr(_1) )
# 565 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt_list) in
    Obj.repr(
# 71 "parser.mly"
                      ( Block(_2) )
# 572 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt_list) in
    Obj.repr(
# 72 "parser.mly"
                                    ( If(_3, _6) )
# 580 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 9 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 6 : Ast.stmt_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 4 : 'elif_stmt_list) in
    let _11 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt_list) in
    Obj.repr(
# 73 "parser.mly"
                                                                       ( IfElif(_3, _6, _8, _11))
# 590 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : Ast.stmt_list) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt_list) in
    Obj.repr(
# 74 "parser.mly"
                                                         ( IfElse(_3, _6, _10))
# 599 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt_list) in
    Obj.repr(
# 75 "parser.mly"
                                       ( While(_3, _6))
# 607 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt_list) in
    Obj.repr(
# 76 "parser.mly"
                                                                   ( For(_3, _5, _7, _10))
# 617 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt_list) in
    Obj.repr(
# 77 "parser.mly"
                                              (FunctionCreation(_2, _6))
# 625 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt_list) in
    Obj.repr(
# 82 "parser.mly"
                                       ([(_3, _6)])
# 633 "parser.ml"
               : 'elif_stmt_list))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.stmt_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'elif_stmt_list) in
    Obj.repr(
# 83 "parser.mly"
                                                     ((_3, _6)::_8)
# 642 "parser.ml"
               : 'elif_stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 86 "parser.mly"
                 ( Lit(_1) )
# 649 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 87 "parser.mly"
               ( FloatLit(_1) )
# 656 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 88 "parser.mly"
               ( BoolLit(_1) )
# 663 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 89 "parser.mly"
                  (Return(_2) )
# 670 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 90 "parser.mly"
                           (Asn(_1, _3))
# 678 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 91 "parser.mly"
               ( Var(_1) )
# 685 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 92 "parser.mly"
                    ( print_endline("Calling func"); FunctionCall(_1) )
# 692 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 94 "parser.mly"
                            ( GraphAccess(_1, "vertices") )
# 699 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 95 "parser.mly"
                         ( GraphAccess(_1, "edges") )
# 706 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'graph_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 96 "parser.mly"
                                  (GraphAsn(_3, _1))
# 714 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'graph_elements_list) in
    Obj.repr(
# 97 "parser.mly"
                                               (GraphOp(_3, _4, "insert"))
# 722 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'graph_elements_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 98 "parser.mly"
                                               (GraphOp(_4, _2, "delete"))
# 730 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'graph_element) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 99 "parser.mly"
                                         ( GraphUpdate(_4, _2) )
# 738 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
               ( Uniop(Not, _2) )
# 745 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                     (Binop(_1, Add, _3) )
# 753 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                      ( Binop(_1, Sub, _3) )
# 761 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "parser.mly"
                      ( Binop(_1, Mul, _3) )
# 769 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                       ( Binop(_1, Div, _3) )
# 777 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
                        ( Binop(_1, Mod, _3) )
# 785 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
                    ( Binop(_1, Eq, _3) )
# 793 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "parser.mly"
                       ( Binop(_1, Neq, _3) )
# 801 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                   ( Binop(_1, Gt, _3) )
# 809 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                   ( Binop(_1, Lt, _3) )
# 817 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "parser.mly"
                     ( Binop(_1, Gteq, _3) )
# 825 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                     ( Binop(_1, Lteq, _3) )
# 833 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                    ( Binop(_1, And, _3) )
# 841 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                   ( Binop(_1, Or, _3) )
# 849 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
                 ( _2 )
# 856 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
           ( _1 )
# 863 "parser.ml"
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
