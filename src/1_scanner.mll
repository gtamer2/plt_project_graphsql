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
| '%' { MODULUS}
| '=' { ASSIGN }
| '<' { LT }
| "<=" { LTEQ }
| '>' { GT }
| ">=" { GTEQ }
| "==" { EQL }
| "!=" { NOTEQL }
| ';' { SEQ }
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| letter (letter | digit | '_')* as id { VARIABLE( id ) }
| "True" { BLIT(true) }
| "False" { BLIT(false) }
| '-'?digit+'.'digit (['e' 'E']['+' '-']? digit ) as fltlit { FLOATLIT(float_of_string fltlit) }
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

| "." { ACCESSOR }
| "vertices" { VERTICES }
| "edges" { EDGES }

| "," {COMMA}
| "\"" {QUOTES}

| "(" { LP }
| ")" { RP }
| "[" { LB }
| "]" { RB }
| "{" { LC }
| "}" { RC }
| "," { COMMA }
| "-" { DASH }
| "->" { ARROW }

| "IF" { IF }
| "ELSE" { ELSE }
| "ELIF" { ELIF }


| eof { EOF }
| "#" { COMMENT }
| _ { raise (Failure "Character not allowed") }