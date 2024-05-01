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
%token CREATE SELECT FROM AS WHERE INSERT UNION INTERSECT APPLY WHILE

%token DOT 
%token VERTEX EDGE VERTICES EDGES
%token LP RP LB RB LC RC COMMA ARROW QUOTES COMMENT
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
    | VERTEX LP VARIABLE RP { Vertex($3) }
    | EDGE LP VARIABLE MINUS VARIABLE COMMA LITERAL RP { Edge($3, $5, $7) }

graph_elements:
    | graph_element COMMA graph_elements { $1::$3 }
    | graph_element { [$1] }
    | /*empty*/ { [] }


graph_elements_list:
    | LB graph_elements RB {$2}


graph_init:
    | CREATE GRAPH LP RP { Graph([]) } //eventually can remove this 
    | CREATE GRAPH LP graph_elements_list RP { Graph($4) }

// graph_accessor:
//     | VERTICES {}
//     | EDGES 
    
    
expr:    
    // NON-RECURSIVE
    | LITERAL    { Lit($1) } //done
    | FLOATLIT { FloatLit($1) } //done
    | BLIT     { BoolLit($1) }
    | VARIABLE ASSIGN expr   {Asn($1, $3)} //done
    | VARIABLE { Var($1) } //done
    | VARIABLE DOT VERTICES { GraphAccess($1, "vertices") } // IN PROGRESS
    | VARIABLE DOT EDGES { GraphAccess($1, "edges") }  // IN PROGRESS
    | graph_init AS VARIABLE { GraphAsn($3, $1)} // DONE
    | expr PLUS expr { Binop($1, Add, $3) } //done
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
    | LP expr RP { $2 }
    | expr AND expr { Binop($1, And, $3) }
    | expr OR expr { Binop($1, Or, $3) }
    | expr SEMICOLON expr { Seq($1, $3) }
    | expr SEMICOLON {$1}

entry:
| expr EOF { $1 }