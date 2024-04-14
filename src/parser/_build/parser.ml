type token =
  | LITERAL of (
# 6 "parser.mly"
        int
# 6 "parser.ml"
)
  | BLIT of (
# 7 "parser.mly"
        bool
# 11 "parser.ml"
)
  | VARIABLE of (
# 8 "parser.mly"
        string
# 16 "parser.ml"
)
  | FLOATLIT of (
# 9 "parser.mly"
        float
# 21 "parser.ml"
)
  | STRINGLIT of (
# 10 "parser.mly"
        string
# 26 "parser.ml"
)
  | GRAPH
  | VERTEX
  | EDGE
  | VERTICES
  | EDGES
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
  | LP
  | RP
  | LB
  | RB
  | LC
  | RC
  | COMMA
  | DASH
  | ARROW
  | ACCESSOR
  | QUOTES
  | COMMENT
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MODULUS
  | ASSIGN
  | SEQ
  | EQL
  | NOTEQL
  | GT
  | LT
  | GTEQ
  | LTEQ
  | AND
  | OR
  | NOT
  | IF
  | ELSE
  | ELIF
  | EOF
  | DEFINE
  | FUNCTION

open Parsing
let _ = parse_error;;
# 3 "parser.mly"
    open Ast
# 82 "parser.ml"
let yytransl_const = [|
  262 (* GRAPH *);
  263 (* VERTEX *);
  264 (* EDGE *);
  265 (* VERTICES *);
  266 (* EDGES *);
  267 (* CREATE *);
  268 (* SELECT *);
  269 (* FROM *);
  270 (* AS *);
  271 (* WHERE *);
  272 (* INSERT *);
  273 (* UNION *);
  274 (* INTERSECT *);
  275 (* APPLY *);
  276 (* WHILE *);
  277 (* LP *);
  278 (* RP *);
  279 (* LB *);
  280 (* RB *);
  281 (* LC *);
  282 (* RC *);
  283 (* COMMA *);
  284 (* DASH *);
  285 (* ARROW *);
  286 (* ACCESSOR *);
  287 (* QUOTES *);
  288 (* COMMENT *);
  289 (* PLUS *);
  290 (* MINUS *);
  291 (* TIMES *);
  292 (* DIVIDE *);
  293 (* MODULUS *);
  294 (* ASSIGN *);
  295 (* SEQ *);
  296 (* EQL *);
  297 (* NOTEQL *);
  298 (* GT *);
  299 (* LT *);
  300 (* GTEQ *);
  301 (* LTEQ *);
  302 (* AND *);
  303 (* OR *);
  304 (* NOT *);
  305 (* IF *);
  306 (* ELSE *);
  307 (* ELIF *);
    0 (* EOF *);
  308 (* DEFINE *);
  309 (* FUNCTION *);
    0|]

let yytransl_block = [|
  257 (* LITERAL *);
  258 (* BLIT *);
  259 (* VARIABLE *);
  260 (* FLOATLIT *);
  261 (* STRINGLIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\001\000\001\000\001\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\005\000\007\000\000\000\006\000\000\000\024\000\
\000\000\000\000\000\000\000\000\000\000\001\000\003\000\000\000\
\000\000\000\000\000\000\000\000\004\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\021\000\010\000\
\011\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000\011\000"

let yysindex = "\009\000\
\005\255\000\000\000\000\000\000\251\254\000\000\005\255\000\000\
\042\000\005\255\153\255\005\255\250\254\000\000\000\000\005\255\
\005\255\005\255\005\255\005\255\000\000\005\255\005\255\005\255\
\005\255\005\255\005\255\005\255\005\255\168\255\000\000\000\000\
\000\000\080\255\080\255\054\255\210\255\210\255\224\254\224\254\
\224\254\224\254\197\255\183\255"

let yyrindex = "\000\000\
\044\000\000\000\000\000\000\000\234\254\000\000\000\000\000\000\
\000\000\044\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\021\255\000\000\000\000\
\000\000\010\255\026\255\037\255\105\255\115\255\053\255\063\255\
\079\255\089\255\118\255\119\255"

let yygindex = "\000\000\
\000\000\048\000\000\000\156\000"

let yytablesize = 255
let yytable = "\008\000\
\016\000\017\000\018\000\019\000\020\000\003\000\004\000\005\000\
\006\000\001\000\008\000\008\000\008\000\008\000\008\000\031\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\007\000\016\000\017\000\018\000\019\000\020\000\012\000\
\012\000\022\000\023\000\024\000\025\000\026\000\027\000\028\000\
\029\000\014\000\009\000\002\000\012\000\012\000\012\000\013\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\015\000\014\000\009\000\013\000\013\000\013\000\000\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\014\000\017\000\014\000\014\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\018\000\000\000\016\000\017\000\
\018\000\019\000\000\000\017\000\017\000\017\000\017\000\017\000\
\017\000\017\000\017\000\017\000\019\000\018\000\018\000\018\000\
\018\000\018\000\018\000\018\000\018\000\018\000\020\000\000\000\
\016\000\017\000\000\000\000\000\000\000\019\000\019\000\019\000\
\019\000\019\000\019\000\019\000\019\000\019\000\015\000\020\000\
\020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
\016\000\000\000\000\000\022\000\023\000\000\000\000\000\015\000\
\015\000\015\000\000\000\000\000\000\000\000\000\015\000\015\000\
\000\000\016\000\016\000\016\000\022\000\023\000\000\000\000\000\
\016\000\016\000\013\000\022\000\022\000\023\000\000\000\030\000\
\000\000\000\000\000\000\032\000\033\000\034\000\035\000\036\000\
\000\000\037\000\038\000\039\000\040\000\041\000\042\000\043\000\
\044\000\016\000\017\000\018\000\019\000\020\000\000\000\021\000\
\022\000\023\000\024\000\025\000\026\000\027\000\028\000\029\000\
\016\000\017\000\018\000\019\000\020\000\000\000\000\000\022\000\
\023\000\024\000\025\000\026\000\027\000\028\000\029\000\016\000\
\017\000\018\000\019\000\020\000\000\000\000\000\022\000\023\000\
\024\000\025\000\026\000\027\000\028\000\016\000\017\000\018\000\
\019\000\020\000\000\000\000\000\022\000\023\000\024\000\025\000\
\026\000\027\000\016\000\017\000\018\000\019\000\020\000\000\000\
\000\000\000\000\000\000\024\000\025\000\026\000\027\000"

let yycheck = "\022\001\
\033\001\034\001\035\001\036\001\037\001\001\001\002\001\003\001\
\004\001\001\000\033\001\034\001\035\001\036\001\037\001\022\001\
\039\001\040\001\041\001\042\001\043\001\044\001\045\001\046\001\
\047\001\021\001\033\001\034\001\035\001\036\001\037\001\022\001\
\038\001\040\001\041\001\042\001\043\001\044\001\045\001\046\001\
\047\001\000\000\022\001\000\000\035\001\036\001\037\001\022\001\
\039\001\040\001\041\001\042\001\043\001\044\001\045\001\046\001\
\047\001\010\000\022\001\039\001\035\001\036\001\037\001\255\255\
\039\001\040\001\041\001\042\001\043\001\044\001\045\001\046\001\
\047\001\037\001\022\001\039\001\040\001\041\001\042\001\043\001\
\044\001\045\001\046\001\047\001\022\001\255\255\033\001\034\001\
\035\001\036\001\255\255\039\001\040\001\041\001\042\001\043\001\
\044\001\045\001\046\001\047\001\022\001\039\001\040\001\041\001\
\042\001\043\001\044\001\045\001\046\001\047\001\022\001\255\255\
\033\001\034\001\255\255\255\255\255\255\039\001\040\001\041\001\
\042\001\043\001\044\001\045\001\046\001\047\001\022\001\039\001\
\040\001\041\001\042\001\043\001\044\001\045\001\046\001\047\001\
\022\001\255\255\255\255\022\001\022\001\255\255\255\255\039\001\
\040\001\041\001\255\255\255\255\255\255\255\255\046\001\047\001\
\255\255\039\001\040\001\041\001\039\001\039\001\255\255\255\255\
\046\001\047\001\007\000\046\001\047\001\047\001\255\255\012\000\
\255\255\255\255\255\255\016\000\017\000\018\000\019\000\020\000\
\255\255\022\000\023\000\024\000\025\000\026\000\027\000\028\000\
\029\000\033\001\034\001\035\001\036\001\037\001\255\255\039\001\
\040\001\041\001\042\001\043\001\044\001\045\001\046\001\047\001\
\033\001\034\001\035\001\036\001\037\001\255\255\255\255\040\001\
\041\001\042\001\043\001\044\001\045\001\046\001\047\001\033\001\
\034\001\035\001\036\001\037\001\255\255\255\255\040\001\041\001\
\042\001\043\001\044\001\045\001\046\001\033\001\034\001\035\001\
\036\001\037\001\255\255\255\255\040\001\041\001\042\001\043\001\
\044\001\045\001\033\001\034\001\035\001\036\001\037\001\255\255\
\255\255\255\255\255\255\042\001\043\001\044\001\045\001"

let yynames_const = "\
  GRAPH\000\
  VERTEX\000\
  EDGE\000\
  VERTICES\000\
  EDGES\000\
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
  LP\000\
  RP\000\
  LB\000\
  RB\000\
  LC\000\
  RC\000\
  COMMA\000\
  DASH\000\
  ARROW\000\
  ACCESSOR\000\
  QUOTES\000\
  COMMENT\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  MODULUS\000\
  ASSIGN\000\
  SEQ\000\
  EQL\000\
  NOTEQL\000\
  GT\000\
  LT\000\
  GTEQ\000\
  LTEQ\000\
  AND\000\
  OR\000\
  NOT\000\
  IF\000\
  ELSE\000\
  ELIF\000\
  EOF\000\
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
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 38 "parser.mly"
                  ( _1 )
# 319 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "parser.mly"
                  ( [] )
# 325 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 42 "parser.mly"
                     ( _1::_2 )
# 333 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 45 "parser.mly"
               ( Expr _1 )
# 340 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 56 "parser.mly"
               ( Literal(_1) )
# 347 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 57 "parser.mly"
               ( FloatLit(_1) )
# 354 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 58 "parser.mly"
               ( BoolLit(_1) )
# 361 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 60 "parser.mly"
                 ( Variable(_1) )
# 368 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 61 "parser.mly"
                             (Assign(_1, _3))
# 376 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 62 "parser.mly"
                     ( Binop(_1, Add, _3) )
# 384 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 63 "parser.mly"
                      ( Binop(_1, Sub, _3) )
# 392 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 64 "parser.mly"
                      ( Binop(_1, Mult, _3) )
# 400 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 65 "parser.mly"
                       ( Binop(_1, Divd, _3) )
# 408 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                        ( Binop(_1, Mod, _3) )
# 416 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                    ( Binop(_1, Eq, _3) )
# 424 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "parser.mly"
                       ( Binop(_1, Neq, _3) )
# 432 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
                   ( Binop(_1, Gt, _3) )
# 440 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 70 "parser.mly"
                   ( Binop(_1, Lt, _3) )
# 448 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                     ( Binop(_1, Gteq, _3) )
# 456 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                     ( Binop(_1, Lteq, _3) )
# 464 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
                 ( _2 )
# 471 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
                    ( Binop(_1, And, _3) )
# 479 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                   ( Binop(_1, Or, _3) )
# 487 "parser.ml"
               : 'expr))
(* Entry program *)
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
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
