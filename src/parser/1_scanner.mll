{ open Parser }

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| '=' { ASSIGN }
| ';' { SEQ }
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| letter (letter | digit | '_')* as id { VARIABLE( id ) }
| "True" { BLIT(true) }
| "False" { BLIT(false) }



| "DEFINE" { DEFINE }
| "FUNCTION" { FUNCTION }

| "CREATE" { CREATE }
| "SELECT" { SELECT }
| "FROM" { FROM }
| "AS" { AS }
| "WHERE" { WHERE }

| "UNION" { UNION }
| "INTERSECT" { INTERSECT }

| "GRAPH" { GRAPH }
| "VERTEX" { VERTEX }
| "EDGE" { EDGE }

| "." { ACCESSOR }
| "vertices" { VERTICES }
| "edges" { EDGES }

| "WHILE" { WHILE }
| "," {COMMA}

| "(" { LP }
| ")" { RP }

| "{" { LB }
| "}" { RB }

| eof { EOF }
| _ { raise (Failure "Character not allowed") }