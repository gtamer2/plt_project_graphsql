# Scanner Test Bank

## Goal

The goal of the scanner test bank is to comprehensively test our compiler's scanning logic, as specified in `src/1_scanner.mll`.

## Walkthrough

Each file represents a logical set of test cases.

### ids

The `id_invalid` and `id_valid` test banks test the scanning rule for variable ids.

### Keywords

The `keywords_valid` and `keywords_wrong_case` test banks test the scanning rule for all of the language's keywords.

Note that the scanner should produce a valid list of tokens for `keywords_wrong_case`, but with each token being a variable id rather than a keyword.

### Literals

`literals_valid` tests the scanning rule for our literal number values.

Note that the "invalid literals" test case is implicitly covered as a valid/invalid id

### Symbols

`symbols_valid` and `symbols_invalid` test banks test the scanning rule for all of the language's symbols.
