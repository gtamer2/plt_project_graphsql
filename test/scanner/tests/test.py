import os
import subprocess
from pathlib import Path

# Define the paths to your testbenches directory, expected output directory, and outputs directory
testbenches_dir = Path("tests/")
expected_outputs_dir = Path("expected_outputs/")
outputs_dir = Path("outputs/")

# Compile your OCaml program
subprocess.run(["make"])

# Loop through each file in the testbenches directory
for test_file in testbenches_dir.glob("*.tb"):
    test_base = test_file.name

    # Check if the testbench file name contains "test"
    if "test" in test_base:
        # Run your OCaml program with the current testbench file and capture output and errors
        with open(outputs_dir / f"{test_file.stem}.out", "w") as output_file, \
             open("temp_error.out", "w") as error_file:
            subprocess.run(["./calc"], stdin=open(test_file, "r"), stdout=output_file, stderr=error_file)
        
        # Check if an error occurred (non-empty error output)
        if os.path.getsize("temp_error.out") > 0:
            print(f"Test case {test_base} produced an error:")
            with open("temp_error.out", "r") as error_file:
                print(error_file.read())
        else:
            # Compare the output with the corresponding expected output
            expected_output = expected_outputs_dir / f"{test_file.stem}.out"
            result = subprocess.run(["diff", "-q", outputs_dir / f"{test_file.stem}.out", expected_output],
                                    stdout=subprocess.PIPE)
            if result.returncode == 0:
                print(f"Test case {test_base} passed.")
            else:
                print(f"Test case {test_base} failed: outputs do not match.")

    elif "fail" in test_base:
        # Run your OCaml program with the current testbench file and capture errors
        with open("temp_error.out", "w") as error_file:
            subprocess.run(["./calc"], stdin=open(test_file, "r"), stderr=error_file)
        
        # Check if an error occurred (non-empty error output)
        if os.path.getsize("temp_error.out") > 0:
            print(f"Test case {test_base} produced an expected error.")
        else:
            print(f"Test case {test_base} did not produce an expected error.")
    else:
        print(f"Unknown test case type: {test_base}")

# Clean up temporary files
os.remove("temp_error.out")
