%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE ASSIGN SEMICOLON MODULUS EOF
%token <int>  LITERAL
%token <bool> BLIT
%token <string> VARIABLE
%token <float> FLOATLIT
%token <string> STRINGLIT
// %token <string> IDENTIFIER //added for graph names

%token EQL NOTEQL GT LT GTEQ LTEQ AND OR NOT
%token GRAPH VERTEX EDGE VERTICES EDGES
%token CREATE SELECT FROM AS WHERE INSERT UNION INTERSECT APPLY WHILE
%token LP RP LB RB LC RC COMMA DASH ARROW ACCESSOR QUOTES COMMENT
%token IF ELSE ELIF
%token DEFINE FUNCTION

%left OR AND
%left SEMICOLON
%right ASSIGN
%left PLUS MINUS
%left TIMES DIVIDE
%left MODULUS

%left EQL NOTEQL
%left GT LT GTEQ LTEQ

// %%

// program:
//   | statements EOF { $1 }

// statements:
//   | statement SEMICOLON statements { Seq($1, $3) }
//   | statement { $1 }

// statement:
//   | expr { $1 }
//   | graph_init { $1 }

// graph_init:
//   | CREATE GRAPH LP RP AS IDENTIFIER { NamedGraph($6, [], []) }


// // entry:
// //   | expr EOF { $1 }


%start expr
%type <Ast.expr> expr

%%

expr:    
    LITERAL    { Lit($1) } //done
    | FLOATLIT { FloatLit($1) } //done
    | BLIT     { BoolLit($1) }
   // | QUOTES STRINGLIT QUOTES { StringLit($2) }
    | VARIABLE   { Var($1) } //done
    | VARIABLE ASSIGN expr   {Asn($1, $3)} //done
    | expr PLUS expr { Binop($1, Add, $3) } //done
    | expr MINUS expr { Binop($1, Sub, $3) } //done
    | expr TIMES expr { Binop($1, Mul, $3) } //done
    | expr DIVIDE expr { Binop($1, Div, $3) } //done
    | expr MODULUS expr { Binop($1, Mod, $3) } //done
    | expr EQL expr { Bool_Binop($1, Eq, $3) }
    | expr NOTEQL expr { Bool_Binop($1, Neq, $3) }
    | expr GT expr { Bool_Binop($1, Gt, $3) }
    | expr LT expr { Bool_Binop($1, Lt, $3) }
    | expr GTEQ expr { Bool_Binop($1, Gteq, $3) }
    | expr LTEQ expr { Bool_Binop($1, Lteq, $3) }
    | LP expr RP { $2 }
    | expr AND expr { Bool_Binop($1, And, $3) }
    | expr OR expr { Bool_Binop($1, Or, $3) }
    | expr SEMICOLON expr { Seq($1, $3) }
    | expr SEMICOLON {$1}


entry:
| expr EOF { $1 }