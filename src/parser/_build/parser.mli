type token =
  | LITERAL of (
# 6 "parser.mly"
        int
# 6 "parser.mli"
)
  | BLIT of (
# 7 "parser.mly"
        bool
# 11 "parser.mli"
)
  | VARIABLE of (
# 8 "parser.mly"
        string
# 16 "parser.mli"
)
  | FLOATLIT of (
# 9 "parser.mly"
        float
# 21 "parser.mli"
)
  | STRINGLIT of (
# 10 "parser.mly"
        string
# 26 "parser.mli"
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

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
