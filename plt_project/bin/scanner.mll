{ open Parser }

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let quote = '\"'

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| ';' { SEMICOLON }
| "." { DOT }
| ('-'?)['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| "vertices" { VERTICES }
| "edges" { EDGES }
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
(* Return types *)
| "Int" { INT }
| "Float" { FLOAT }
| "Boolean" { BOOLEAN }
(* Return types *)
| "True" { BLIT(true) }
| "False" { BLIT(false) }
| '-'?digit*'.'digit* as fltlit { FLOATLIT(float_of_string fltlit) }
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
| letter (letter | digit | '_')* as id { VARIABLE( id ) }
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
