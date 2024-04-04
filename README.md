## Repository breakdown

**`/test` directory**: contains GraphSQL source code that demonstrates important features.
**`/src` directory**: contains the compiler implementation, including scanner and parser code, AST definitions, and AST pretty printer

## Work Done so Far
Scanner: primitive and derived data types, operators, keywords, separators, comments
Parser: functions, graph statements, graph expressions
Ast: vertex, edges, expressions function types defined
Basic Scanner Unit tests implemented

## Work Left Until Completion
1. More test cases for front-end (scanner, parser, ast), both unit tests and integration tests
2. back-end implementation (graphsql_eval)

## Timeline
1. Test cases and debugging for front-end -- April 8 
2. back-end implmentation 
    2.1 data types, variable assignment (memory layout of lists, etc.)
    2.2 functions, anonymous functions
    2.1-2.2 -- April 15 
    2.3 more complex graph statements & operations
    2.4 semantic checking & type checking 
    2.3-2.4 -- April 22
3. test cases for back-end -- April 29
4. more integrational-level test cases -- May 5
5. final report -- May 9

- IR gen for data types (primitive, lists, etc) and variable assignment. For instance, for lists you also have to think about the memory layout of lists (contiguous memory? linked lists?).
- IR gen for functions, anonymous functions etc
- IR gen for classes, if your language supports that
- Complex semantic checking / type systems if your language supports that


## Order of operations

1. "lexer" = scanner.mll
2. parser -> parser.mly
3.
