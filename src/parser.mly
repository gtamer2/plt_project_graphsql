
%{
    open Ast
%}

%token <int>  LITERAL
%token <bool> BLIT
%token <string> VARIABLE
%token <float> FLOATLIT
%token <string> STRINGLIT
%token GRAPH VERTEX EDGE VERTICES EDGES
%token CREATE SELECT FROM AS WHERE INSERT UNION INTERSECT APPLY WHILE
%token LP RP LB RB LC RC COMMA DASH ARROW ACCESSOR QUOTES COMMENT
%token PLUS MINUS TIMES DIVIDE MODULUS ASSIGN SEQ EQL NOTEQL GT LT GTEQ LTEQ AND OR NOT
%token IF ELSE ELIF
%token EOF

%token DEFINE FUNCTION

%right ASSIGN
%left OR
%left AND
%left EQL NOTEQL
%left GT LT GTEQ LTEQ
%left MODULUS
%left TIMES DIVIDE
%left PLUS MINUS

%start expr
%type <Ast.expr> expr

%%


/* initialization */
(*program:
    stmt_list EOF { $1 }

stmt_list:
    /* nothing */ { [] }
    | stmt stmt_list { $1::$2 }

stmt:
    expr SEQ   { Expr $1 }
    
   // | graph_stmt SEQ { $1 }
   /* | LC stmt_list RC { Block $2 }
    | WHILE expr stmt_list { While($2, $3) }
    | IF expr stmt_list ELSE stmt_list { If($2, $3, $5) }
    | RETURN expr SEQ { Return $2 }
    //| typ expr SEQ { ($1, $2) }
    | SEQ { } /* null statement */
*)

expr:
    LITERAL    { Literal($1) } //done
    | FLOATLIT { FloatLit($1) } //done
    | BLIT     { BoolLit($1) }
   // | QUOTES STRINGLIT QUOTES { StringLit($2) }
    | VARIABLE   { Variable($1) } //done
    | VARIABLE ASSIGN expr   {Assign($1, $3)} //done
    | expr PLUS expr { Binop($1, Add, $3) } //done
    | expr MINUS expr { Binop($1, Sub, $3) } //done
    | expr TIMES expr { Binop($1, Mult, $3) } //done
    | expr DIVIDE expr { Binop($1, Divd, $3) } //done
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