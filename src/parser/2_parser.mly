
%{
    open Ast
%}

%token DEFINE FUNCTION LP RP LB RB LC RC COMMA DASH ARROW
%token CREATE SELECT FROM AS WHERE INSERT UNION INTERSECT APPLY WHILE
%token GRAPH VERTEX EDGE
%token SEQ ASSIGN
%token <int>  LITERAL
%token <bool> BLIT
%token <string> VARIABLE
%token EOF


// %left PLUS MINUS
// %left TIMES DIVIDE

%start program
%type <Ast.program> program

%%

/* TODO: need to figure out how to do things without forward declaration */

/* initialization */
program:
    stmt_list EOF { $1 }

/* basic statement and expression structures */

stmt_list:
    /* nothing */ { [] }
    | stmt stmt_list { $1::$2 }

stmt:
    expr SEQ   { Expr $1 }
    | graph_stmt SEQ { $1 }

expr:
    LITERAL    { Literal($1) }
    | VARIABLE   { Variable($1) }
    | VARIABLE ASSIGN expr   {Assign($1, $3)}

fdef:
    DEFINE FUNCTION VARIABLE LP formals_opt RP LB stmt_list RB
    {
        {
            rtype=
            fname=$3
            formals=$5
            body=$7
        }
    }

graph_stmt:
    CREATE GRAPH LP graph_args_opt RP AS VARIABLE 
    {
        {

        }
    }
    /* inserts, deletes etc */

graph_args_opt:
    /* nothing */ { [] }
    | graph_args { $1 }

graph_args:
    graph_expr { [$1] }
    | graph_expr COMMA graph_args { $1::$3 }

graph_expr:
    VERTEX LP vertex RP { ($3) }
    | EDGE LP edge_args RP { ($3) }

edge_args:
    vertex direction vertex 
    {
        {
            vertex1=$1;
            vertex2=$3;
            direction_type=$2;
            weight_type=Unweighted
        }
    } /* unweighted */
    | vertex direction vertex COMMA weight 
    {
        {
            vertex1=$1;
            vertex2=$3;


        }
    } /* weighted */

vertex:
    LITERAL {VerLiteral($1)}
    | BLIT  {VerBoolLit($1)}
    | /* float */
    | /* string */


/* instructions splits into declarations, statements, etc*/

// BINARY OPS
// query:
//     CREATE GRAPH LP createexpr RP AS VARIABLE {CreateGraph(G, list of vertixes, list of edges)}

// createexpr:
//     VERTEX LP LITERAL RP 
//     | VERTEX LP LITERAL RP COMMA 
//     | EDGE  LP LITERAL RP 
//     | EDGE  LP LITERAL RP COMMA 
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