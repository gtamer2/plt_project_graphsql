{ open Parser }

(* Standard letter/digit. *)
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let quote = '\"'

rule tokenize = parse
(* Whitespace insensitive language *)
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
(* Basic arithmetic operators *)
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| ';' { SEMICOLON }
| "." { DOT }
(* Ints and Floats *)
| ('-'?)['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| '-'?digit*'.'digit* as fltlit { FLOATLIT(float_of_string fltlit) }
(* Other operators *)
| eof { EOF }
| '%' { MODULUS}
| '<' { LT }
| "<=" { LTEQ }
| '>' { GT }
| ">=" { GTEQ }
| "==" { EQL }
| "!=" { NOTEQL }
| ":" {COLON}
| ';' { SEMICOLON }
| '=' { ASSIGN }
(* Return types - start *)
| "Int" { INT }
| "Float" { FLOAT }
| "Boolean" { BOOLEAN }
(* Return types -end *)
(* Keywords - start *)
| "vertices" { VERTICES }
| "edges" { EDGES }
| "True" { BLIT(true) }
| "False" { BLIT(false) }
| "return" { RETURN }
| "LAMBDA" {LAMBDA}
| "AND" { AND }
| "OR" { OR }
| "DEFINE" { DEFINE }
| "FUNCTION" { FUNCTION }
| "CREATE" { CREATE }
| "SELECT" { SELECT }
| "FROM" { FROM }
| "UNION" { UNION }
| "INTERSECT" { INTERSECT }
| "UPDATE" {UPDATE}
| "AS" { AS }
| "WHERE" { WHERE }
| "INSERT" { INSERT }
| "INTO" { INTO }
| "DELETE" { DELETE }
| "APPLY" { APPLY }
| "GRAPH" { GRAPH }
| "VERTEX" { VERTEX }
| "EDGE" { EDGE }
| "NOT" { NOT }
| "WHILE" { WHILE }
| "FOR" { FOR }
| "IF" { IF }
| "ELSE" { ELSE }
| "ELIF" { ELIF }
(* Keywords - end *)
| letter (letter | digit | '_')* as id { VARIABLE( id ) }
(* Miscellaneous *)
| "," {COMMA}
| "\"" {QUOTES}
| "(" { LP }
| ")" { RP }
| "[" { LB }
| "]" { RB }
| "{" { LC }
| "}" { RC }
| "," { COMMA }
| "->" { ARROW }
| '#' [^ '\n']* { tokenize lexbuf }
| _ { raise (Failure "Character not allowed") }
