%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE ASSIGN SEMICOLON MODULUS EOF
%token <int>  LITERAL
%token <bool> BLIT
%token <string> VARIABLE
%token <float> FLOATLIT
%token <string> STRINGLIT

%token EQL NOTEQL GT LT GTEQ LTEQ AND OR NOT
%token CREATE SELECT FROM AS WHERE INSERT UNION INTERSECT APPLY WHILE
%token GRAPH VERTEX EDGE VERTICES EDGES
%token LP RP LB RB LC RC COMMA DASH ARROW ACCESSOR QUOTES COMMENT
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

// %start expr
// %type <Ast.expr> expr
// %start program
// %type <Ast.expr> program

// %%

// program:
//   | statements EOF { $1 }

// statements:
//   | statement SEMICOLON statements { Seq($1, $3) }
//   | statement { $1 }

// statement:
//   | expr { $1 }
//   | graph_init { $1 }


%start expr
%type <Ast.expr> expr

%%

graph_element:
    | VERTEX LP VARIABLE RP { Vertex($3) }
    //| EDGE LP VARIABLE DASH VARIABLE COMMA FLOATLIT RP { Edge($3, $5, $7) } //weighted undirected 
    | EDGE LP VARIABLE DASH VARIABLE RP { Edge($3, $5) } //unweighted undirected


graph_elements:
    | graph_element COMMA graph_elements { $1::$3 }
    | graph_element


graph_elements_list:
    | LB graph_elements RB
    | _

graph_init:
    | CREATE GRAPH LP RP { Graph([]) }
    | CREATE GRAPH LP graph_elements_list RP { Graph([$4]) }

expr:    
    // NON-RECURSIVE
    | LITERAL    { Lit($1) } //done
    | FLOATLIT { FloatLit($1) } //done
    | BLIT     { BoolLit($1) }
    | VARIABLE ASSIGN expr   {Asn($1, $3)} //done
    | VARIABLE { Var($1) } //done
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
    | graph_init AS VARIABLE { GraphAsn($3, $1)} // THIS MIGHT BE AN ISSUE

entry:
| expr EOF { $1 }