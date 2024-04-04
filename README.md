## Repository breakdown

**`/test` directory**: contains GraphSQL source code that demonstrates important features.  
**`/src` directory**: contains the compiler implementation, including scanner and parser code, AST definitions, and AST pretty printer  

## Work Done so Far
Scanner: primitive and derived data types, operators, keywords, separators, comments  
Parser: functions, graph statements, graph expressions  
Ast: vertex, edges, expressions function types defined  
Basic Scanner Unit tests implemented  

## Work Left Until Completion
1. More test cases for Front-End (scanner, parser, ast), both unit tests and integration tests
2. Back-End Implementation (graphsql_eval)

## Timeline
1. Test Cases and Debugging for front-end -- April 8 
2. Back-end Implementation:     
    2.1 Data types and variable assignment (memory layout of lists, etc.)  
    2.2 Functions and anonymous functions  
          2.1-2.2 -- April 15   
    2.3 More complex graph statements & operations  
    2.4 Semantic checking & type checking   
          2.3-2.4 -- April 22  
3. Test Cases for Back-End -- April 29  
4. More Integrational-level test cases -- May 5  
5. Final Report -- May 9  


## Order of operations

1. scanner.mll
2. parser.mly
3. ast.ml
