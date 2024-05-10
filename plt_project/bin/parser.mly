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
%token CREATE SELECT FROM AS WHERE INSERT INTO DELETE UNION INTERSECT UPDATE APPLY WHILE FOR RETURN

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
%right NOT
%left GT LT GTEQ LTEQ

%start stmt_list
%type <Ast.stmt_list> stmt_list

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

stmt_list: 
    /* nothing */ { [] }
    | stmt stmt_list {$1::$2 }

// arg_list_definition:
//     | /* nothing */ { [] }
//     | VARIABLE COMMA arg_list_definition { $1::$3 }

stmt:
    | expr SEMICOLON { Expr($1) }
    // | RETURN expr SEMICOLON {Return($2) }
    | LC stmt_list RC { Block($2) }
    | IF LP expr RP LC stmt_list RC { If($3, $6) }
    | IF LP expr RP LC stmt_list RC elif_stmt_list ELSE LC stmt_list RC{ IfElif($3, $6, $8, $11)}
    | IF LP expr RP LC stmt_list RC ELSE LC stmt_list RC { IfElse($3, $6, $10)}
    | WHILE LP expr RP LC stmt_list RC { While($3, $6)}
    | FOR LP expr SEMICOLON expr SEMICOLON expr RP LC stmt_list RC { For($3, $5, $7, $10)}
    | FUNCTION VARIABLE LP RP LC stmt_list RC {FunctionCreation($2, $6)}
    // | FUNCTION VARIABLE LP RP LC stmt_list RETURN expr SEMICOLON RC { print_endline("Creating func def"); FunctionCreation($2, $6)}
    // | DEFINE FUNCTION VARIABLE LP arg_list_definition RP LC stmt_list RC { FunctionCreation($3, $5, $8)}

elif_stmt_list:
    | ELIF LP expr RP LC stmt_list RC  {[($3, $6)]}
    | ELIF LP expr RP LC stmt_list RC elif_stmt_list {($3, $6)::$8}

expr:    
    | LITERAL    { Lit($1) } //done
    | FLOATLIT { FloatLit($1) } //done
    | BLIT     { BoolLit($1) }
    | RETURN expr {Return($2) }
    | VARIABLE ASSIGN expr {Asn($1, $3)} //done
    | VARIABLE { Var($1) } //done
    | VARIABLE LP RP{ print_endline("Calling func"); FunctionCall($1) }
    //| VARIABLE LP arg_list RP { FunctionCall($1, $3) }
    | VARIABLE DOT VERTICES { GraphAccess($1, "vertices") } // done. TODO: check if need to change order?
    | VARIABLE DOT EDGES { GraphAccess($1, "edges") }  // done
    | graph_operation AS VARIABLE {GraphAsn($3, $1)} // DONE
    | INSERT INTO VARIABLE graph_elements_list {GraphOp($3, $4, "insert")}
    | DELETE graph_elements_list FROM VARIABLE {GraphOp($4, $2, "delete")}
    | UPDATE graph_element FROM VARIABLE { GraphUpdate($4, $2) }
    | NOT expr { Uniop(Not, $2) }
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


entry:
| expr EOF { $1 }