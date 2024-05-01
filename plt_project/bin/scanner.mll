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
| '=' { ASSIGN }
| "." { DOT }
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
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
| ';' { SEMICOLON }
| "True" { BLIT(true) }
| "False" { BLIT(false) }
(* | '-'?digit+'.'digit (['e' 'E']['+' '-']? digit ) as fltlit { FLOATLIT(float_of_string fltlit) } *)
(* | quote[ -~]quote as str { STRINGLIT(str) } *)
| "AND" { AND }
| "OR" { OR }
| "DEFINE" { DEFINE }
| "FUNCTION" { FUNCTION }
| "CREATE" { CREATE }
| "SELECT" { SELECT }
| "FROM" { FROM }
| "AS" { AS }
| "WHERE" { WHERE }
| "INSERT" { INSERT }
| "UNION" { UNION }
| "INTERSECT" { INTERSECT }
| "APPLY" { APPLY }
| "GRAPH" { GRAPH }
| "VERTEX" { VERTEX }
| "EDGE" { EDGE }
| "NOT" { NOT }
| "WHILE" { WHILE }
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
| _ { raise (Failure "Character not allowed") }
| "#" { COMMENT }