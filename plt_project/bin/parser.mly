%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE 
%token ASSIGN 
%token SEMICOLON MODULUS COLON EOF

%token <int>  LITERAL
%token <bool> BLIT
%token <string> VARIABLE
%token <float> FLOATLIT
%token <string> STRINGLIT

%token EQL NOTEQL GT LT GTEQ LTEQ AND OR NOT
%token INT BOOLEAN FLOAT
%token CREATE SELECT FROM AS WHERE INSERT INTO DELETE UNION INTERSECT UPDATE APPLY WHILE FOR RETURN 

%token QUOTES
%token DOT 
%token VERTEX EDGE VERTICES EDGES
%token LP RP LB RB LC RC COMMA ARROW COMMENT
%token GRAPH
%token IF ELSE ELIF
%token DEFINE FUNCTION LAMBDA

%left SEMICOLON
%right ASSIGN
%left PLUS MINUS
%left TIMES DIVIDE
%left MODULUS
%left OR AND
%left EQL NOTEQL
%right NOT
%left GT LT GTEQ LTEQ

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1}

decls:
   /* nothing */ { ([], []) }
| stmt_list decls { (($1 @ fst $2), snd $2 )}
| function_declaration decls {(fst $2 , ($1 :: snd $2))  }


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
    | CREATE GRAPH LP RP { Graph([]) }
    | CREATE GRAPH LP graph_elements_list RP { Graph($4) }
    | SELECT VARIABLE DOT VERTICES FROM VARIABLE { GraphAccess($6, "vertices") }
    | SELECT VARIABLE DOT EDGES FROM VARIABLE { GraphAccess($6, "edges") }
    | LP VARIABLE UNION VARIABLE RP { GraphQuery($2, $4, "union") }
    | LP VARIABLE INTERSECT VARIABLE RP { GraphQuery($2, $4, "intersect") }

stmt_list: 
    /* nothing */ { [] }
    | stmt stmt_list { $1 :: $2 }

vdecl_list:
  /*nothing*/ { [] }
  | vdecl SEMICOLON vdecl_list  {  $1 :: $3 }

vdecl:
  unified_type VARIABLE { ($1, $2) }

unified_type:
| INT    { Int }
| FLOAT { Float }
| BOOLEAN { Bool }
//   | STRING { String }
//   | GRAPH { GraphType }

formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }

formals_list:
  vdecl { [$1] }
  | vdecl COMMA formals_list { $1::$3 }

stmt:
    | expr SEMICOLON { Expr($1) }
    | LC stmt_list RC { Block($2) }
    | IF LP expr RP LC stmt_list RC { If($3, $6) }
    | IF LP expr RP LC stmt_list RC elif_stmt_list ELSE LC stmt_list RC{ IfElif($3, $6, $8, $11)}
    | IF LP expr RP LC stmt_list RC ELSE LC stmt_list RC { IfElse($3, $6, $10)}
    | WHILE LP expr RP LC stmt_list RC { While($3, $6)}
    | FOR LP expr SEMICOLON expr SEMICOLON expr RP LC stmt_list RC { For($3, $5, $7, $10)}

function_declaration:
    DEFINE FUNCTION unified_type VARIABLE LP formals_opt RP LC stmt_list RC
    {
        {
            fname=$4;
            formals=$6;
            body=$9;
            rtyp=$3;
        }
    }

elif_stmt_list:
    | ELIF LP expr RP LC stmt_list RC  {[($3, $6)]}
    | ELIF LP expr RP LC stmt_list RC elif_stmt_list {($3, $6)::$8}

args_opt:
  { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }

expr:    
    | LITERAL    { Lit($1) }
    | FLOATLIT { FloatLit($1) }
    | BLIT     { BoolLit($1) }
    | VARIABLE ASSIGN expr {Asn($1, $3)}
    | VARIABLE { Var($1) }
    | VARIABLE LP args_opt RP{FunctionCall($1, $3) } 
    | LAMBDA COLON LP expr RP {LambaFunction($4)} 
    | RETURN expr {Return($2) }
    | VARIABLE DOT VERTICES { GraphAccess($1, "vertices") }
    | VARIABLE DOT EDGES { GraphAccess($1, "edges") }
    | graph_operation AS VARIABLE {GraphAsn($3, $1)}
    | INSERT INTO VARIABLE graph_elements_list {GraphOp($3, $4, "insert")}
    | DELETE graph_elements_list FROM VARIABLE {GraphOp($4, $2, "delete")}
    | UPDATE graph_element FROM VARIABLE { GraphUpdate($4, $2) }
    | NOT expr { Uniop(Not, $2) }
    | expr PLUS expr {Binop($1, Add, $3) }
    | expr MINUS expr { Binop($1, Sub, $3) }
    | expr TIMES expr { Binop($1, Mul, $3) }
    | expr DIVIDE expr { Binop($1, Div, $3) }
    | expr MODULUS expr { Binop($1, Mod, $3) }
    | expr EQL expr { Binop($1, Eq, $3) }
    | expr NOTEQL expr { Binop($1, Neq, $3) }
    | expr GT expr { Binop($1, Gt, $3) }
    | expr LT expr { Binop($1, Lt, $3) }
    | expr GTEQ expr { Binop($1, Gteq, $3) }
    | expr LTEQ expr { Binop($1, Lteq, $3) }
    | expr AND expr { Binop($1, And, $3) }
    | expr OR expr { Binop($1, Or, $3) }
    | LP expr RP { $2 }
