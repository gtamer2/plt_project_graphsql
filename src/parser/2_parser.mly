%token <int> LITERAL
%token <string> VARIABLE
%token GRAPH VERTEX EDGE
%token CREATE SELECT FROM AS WHERE INSERT UNION INTERSECT APPLY WHILE
%token LP RP LB RB LC RC COMMA DASH ARROW
%token PLUS MINUS TIMES DIVIDE ASSIGN SEQ EQL GT LT

%left PLUS MINUS
%left TIMES DIVIDE

%start expr
%type <Ast.expr> expr

%%
// grammar rules for graph initialization 
query:
    CREATE GRAPH LP graph_elements RP AS VARIABLE {CreateGraph(G, list of vertixes, list of edges)}


graph_elements:
    | /* empty */       { [] }
    | graph_element                 { [$1] }
    | graph_element COMMA graph_elements { $1 :: $3 }

graph_element:
    | VERTEX LP LITERAL RP  { Ast.Vertex($3) }
    | EDGE LP VARIABLE DASH VARIABLE COMMA LITERAL RP { Ast.Edge($3, $5, $7) }
    | EDGE LP VARIABLE ARROW VARIABLE COMMA LITERAL RP { Ast.Edge($3, $5, $7) }


CREATE GRAPH () AS g; # initialization of an empty graph 

CREATE GRAPH (
	VERTEX ("Vertex1"),
	VERTEX ("Vertex2"),
	EDGE ("Vertex1" - "Vertex2", 5),
) AS g;


expr2:
    expr2
    | expr2 UNION expr2 { BinGraphOp($1, Union, $3) }
    | expr2 INTERSECT expr2 { BinGraphOp($1, Intersect, $3) }
    
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