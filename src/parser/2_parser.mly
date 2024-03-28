%token <int>  LITERAL

%left PLUS MINUS
%left TIMES DIVIDE

%start expr
%type <Ast.expr> expr

%%
// BINARY OPS
expr:
    expr
    | expr UNION expr { BinGraphOp($1, Union, $3) }
    | expr INTERSECT expr { BinGraphOp($1, Intersect, $3) }
    
    // | expr ACCESSOR VERTICES
    // | expr ACCESSOR EDGES

// UNARY OPS DRAFT
// expr:
//     SELECT * FROM VARIABLE SEQ { Var($1) }
// | expr PLUS   expr { Binop($1, Add, $3) }
// | SELECT expr FROM { Unop($1, Select) }


// CREATION DRAFT
// expr:
    // | CREATE GRAPH (expr) AS VARIABLE; // maybe ID
    // | CREATE VERTEX...
    // | CREATE EDGE ...
    // | NULL



// expr:
// // | expr EOF
// | LITERAL          { Lit($1) }
// | VARIABLE         { Var($1) }
// | VARIABLE ASSIGN expr { Asn($1, $3) }
// | expr SEQ expr { Seq($1, $3) }

// | expr MINUS  expr { Binop($1, Sub, $3) }
// | expr TIMES  expr { Binop($1, Mul, $3) }
// | expr DIVIDE expr { Binop($1, Div, $3) }