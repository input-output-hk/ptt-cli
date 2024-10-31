#!/bin/bash

NIX_PROJECT_PATH="$projectPath"
cd $$NIX_PROJECT_PATH

# Find the first .cabal file in the current directory
CABAL_FILE=$(find . -maxdepth 1 -name "*.cabal" | head -n 1)

# Check if a .cabal file was found
if [[ -z "$$CABAL_FILE" ]]; then
    echo "Error: No .cabal file found in the current directory."
    exit 1
fi

# Extract names of executables and test suites
TEST_SUITES=$(grep -E '^test-suite ' "$$CABAL_FILE" | sed 's/^test-suite //')


# Count the number of test suites
TEST_SUITE_COUNT=$(echo "$$TEST_SUITES" | wc -l)

# Check if there is exactly one test suite
if [[ "$$TEST_SUITE_COUNT" -eq 0 ]]; then
    echo "Error: No test suites found in $$CABAL_FILE."
    exit 1
elif [[ "$$TEST_SUITE_COUNT" -gt 1 ]]; then
    echo "Error: More than one test suite found in $$CABAL_FILE."
    exit 1
else
    echo "$$TEST_SUITES"
fi
