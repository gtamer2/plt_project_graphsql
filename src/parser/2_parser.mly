
%{
    open Ast
%}

%token SEQ ASSIGN
%token <int>  LITERAL
%token <string> VARIABLE
%token EOF

// %left PLUS MINUS
// %left TIMES DIVIDE

%start program
%type <Ast.program> program

%%

/* initialization */
program:
    stmt_list EOF { $1 }

/* basic statement and expression structures */

stmt_list:
    /* nothing */ { [] }
    | stmt stmt_list { $1::$2 }

stmt:
    expr SEQ   { Expr $1 }

expr:
    LITERAL    { Literal($1) }
    | VARIABLE   { Variable($1) }
    | VARIABLE ASSIGN expr   {Assign($1, $3)}


/* instructions splits into declarations, statements, etc*/

// BINARY OPS
// query:
//     CREATE GRAPH LP createexpr RP AS VARIABLE {CreateGraph(G, list of vertixes, list of edges)}

// createexpr:
//     VERTEX LP LITERAL RP 
//     | VERTEX LP LITERAL RP COMMA 
//     | EDGE  LP LITERAL RP 
//     | EDGE  LP LITERAL RP COMMA 

// expr2:
//     expr2
//     | expr2 UNION expr2 { BinGraphOp($1, Union, $3) }
//     | expr2 INTERSECT expr2 { BinGraphOp($1, Intersect, $3) }
    
    // | expr ACCESSOR VERTICES
    // | expr ACCESSOR EDGES

// UNARY OPS DRAFT
// expr:
//     SELECT * FROM VARIABLE SEQ { Var($1) }
// | expr PLUS   expr { Binop($1, Add, $3) }
// | SELECT expr FROM { Unop($1, Select) }


// CREATION DRAFT
// expr:
//     | CREATE GRAPH (expr) AS VARIABLE; // maybe ID
//     | CREATE VERTEX...
//     | CREATE EDGE ...
//     | NULL



// expr:
// // | expr EOF
// | LITERAL          { Lit($1) }
// | VARIABLE         { Var($1) }
// | VARIABLE ASSIGN expr { Asn($1, $3) }
// | expr SEQ expr { Seq($1, $3) }

// | expr MINUS  expr { Binop($1, Sub, $3) }
// | expr TIMES  expr { Binop($1, Mul, $3) }
// | expr DIVIDE expr { Binop($1, Div, $3) }