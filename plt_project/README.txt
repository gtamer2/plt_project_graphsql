### GraphSQL ###

=== Versions Used ===
LLVM: 14.0.6
Ocaml: 4.13.1

=== Steps to Run ===

=== Automated Testing ===
Make the test scripts executable:

chmod 777 test_irgen.sh;
chmod 777 test_semcheck.sh;

Run the tests:

./test_irgen.sh #tests IR generation
./test_semcheck.sh #tests semantic checking

=== Testing Notes ===

There are two cases for testing:

-> If the test file name contains "fail", the test checks for errors (i.e an error is expected to be thrown).
-> If the test file name contains "test", there are two possibilities:
    -> The test compiles but the output is different from the expected_outputs.
    -> The test doesn't compile or throws errors.

Three folders are associated with testing:
tests: Add test files (.tb) here.
outputs: Outputs from the compiler is stored here.
expected_outputs: Add expected output files here (ground truths). The bash script checks the difference between outputs and expected_outputs.

=== Manual Compilation and Testing on a Single Testcase ===

Switch to the bin directory:

cd bin;
make clean;
make;

Test IR generation:
./graphsql_compile.native <test_case.tb> test_case.out; #replace with specific IR generation test case

Test semantic checking:
./semant_eval <test_case.tb> test_case.out; #replace with specific semantic checking test case