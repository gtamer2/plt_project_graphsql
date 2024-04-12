#!/bin/bash

# Define the paths to your testbenches directory and expected output directory
testbenches_dir="tests/"
expected_outputs_dir="expected_outputs/"
outputs_dir="outputs/"

# Compile your OCaml program
make

# Loop through each file in the testbenches directory
for test_file in ${testbenches_dir}*.tb; do
    # Extract the base name of the test file without the directory
    test_base=$(basename "$test_file")

    # Check if the testbench file name contains "test"
    if [[ $test_base == *"test"* ]]; then
        # Run your OCaml program with the current testbench file and capture output
        ./calc < "${testbenches_dir}${test_base}" > "${outputs_dir}${test_base%.*}.out" 2> temp_error.out
        
        # Check if an error occurred (non-empty error output)
        if [ -s temp_error.out ]; then
            echo "Test case ${test_base} produced an error:"
            cat temp_error.out  # Print the error message
        else
            # Compare the output with the corresponding expected output
            expected_output="${expected_outputs_dir}${test_base%.*}.out"
            if diff -q "${outputs_dir}${test_base%.*}.out" "${expected_output}" >/dev/null; then
                echo "Test case ${test_base} passed."
            else
                echo "Test case ${test_base} failed: outputs do not match."
            fi
        fi
    elif [[ $test_base == *"fail"* ]]; then
        # Run your OCaml program with the current testbench file and capture errors
        ./calc < "${testbenches_dir}${test_base}" 2> temp_error.out
        
        # Check if an error occurred (non-empty error output)
        if [ -s temp_error.out ]; then
            echo "Test case ${test_base} produced an expected error."
        else
            echo "Test case ${test_base} did not produce an expected error."
        fi
    else
        echo "Unknown test case type: ${test_base}"
    fi
done

# Clean up temporary files
rm -f temp_error.out
