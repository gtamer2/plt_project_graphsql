%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE 
%token ASSIGN 
%token SEMICOLON MODULUS EOF

%token <int>  LITERAL
%token <bool> BLIT
%token <string> VARIABLE
%token <float> FLOATLIT
%token <string> STRINGLIT

%token EQL NOTEQL GT LT GTEQ LTEQ AND OR NOT
%token CREATE SELECT FROM AS WHERE INSERT INTO DELETE UNION INTERSECT APPLY WHILE

%token QUOTES
%token DOT 
%token VERTEX EDGE VERTICES EDGES
%token LP RP LB RB LC RC COMMA ARROW COMMENT
%token GRAPH
%token IF ELSE ELIF
%token DEFINE FUNCTION

%left SEMICOLON
%right ASSIGN
%left PLUS MINUS
%left TIMES DIVIDE
%left MODULUS
%left OR AND
%left EQL NOTEQL
%left GT LT GTEQ LTEQ



%start expr
%type <Ast.expr> expr

%%

graph_element:
    | VERTEX LP QUOTES VARIABLE QUOTES RP { Vertex($4) }
    | EDGE LP QUOTES VARIABLE QUOTES MINUS QUOTES VARIABLE QUOTES COMMA LITERAL RP { Edge($4, $8, $11) }

graph_elements:
    | graph_element COMMA graph_elements { $1::$3 }
    | graph_element { [$1] }
    | /*empty*/ { [] }


graph_elements_list:
    | LB graph_elements RB {$2}


graph_operation:
    | CREATE GRAPH LP RP { Graph([]) } //eventually can remove this 
    | CREATE GRAPH LP graph_elements_list RP { Graph($4) }
    | SELECT VARIABLE DOT VERTICES FROM VARIABLE { GraphAccess($6, "vertices") }
    | SELECT VARIABLE DOT EDGES FROM VARIABLE { GraphAccess($6, "edges") }
    | LP VARIABLE UNION VARIABLE RP { GraphQuery($2, $4, "union") }
    | LP VARIABLE INTERSECT VARIABLE RP { GraphQuery($2, $4, "intersect") }

expr:    
    // NON-RECURSIVE
    | LITERAL    { Lit($1) } //done
    | FLOATLIT { FloatLit($1) } //done
    | BLIT     { BoolLit($1) }
    | VARIABLE ASSIGN expr {Asn($1, $3)} //done
    | VARIABLE { Var($1) } //done
    | VARIABLE DOT VERTICES { GraphAccess($1, "vertices") } // done. TODO: check if need to change order?
    | VARIABLE DOT EDGES { GraphAccess($1, "edges") }  // done
    | graph_operation AS VARIABLE {GraphAsn($3, $1)} // DONE
    | INSERT INTO VARIABLE graph_elements_list {GraphOp($3, $4, "insert")}
    | DELETE graph_elements_list FROM VARIABLE {GraphOp($4, $2, "delete")}
    | expr PLUS expr {Binop($1, Add, $3) } //done
    | expr MINUS expr { Binop($1, Sub, $3) } //done
    | expr TIMES expr { Binop($1, Mul, $3) } //done
    | expr DIVIDE expr { Binop($1, Div, $3) } //done
    | expr MODULUS expr { Binop($1, Mod, $3) } //done
    | expr EQL expr { Binop($1, Eq, $3) }
    | expr NOTEQL expr { Binop($1, Neq, $3) }
    | expr GT expr { Binop($1, Gt, $3) }
    | expr LT expr { Binop($1, Lt, $3) }
    | expr GTEQ expr { Binop($1, Gteq, $3) }
    | expr LTEQ expr { Binop($1, Lteq, $3) }
    | expr AND expr { Binop($1, And, $3) }
    | expr OR expr { Binop($1, Or, $3) }
    | LP expr RP { $2 } //should this be moved
    | expr SEMICOLON expr { Seq($1, $3) }
    | expr SEMICOLON {$1}
    | IF LP expr RP LC expr RC { print_endline "Parsing if"; If($3, $6)}
    | IF LP expr RP LC expr RC ELSE LC expr RC { print_endline "Parsing if/else"; IfElse($3, $6, $10)}

entry:
| expr EOF { $1 }