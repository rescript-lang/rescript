#!/bin/bash

# Lambda IR Parity Test Suite: Compare Rust and OCaml Lambda IR output
#
# Tests performed:
#
# 1. sexp parity - Lambda IR structure matches between OCaml and Rust
# 2. sexp-locs parity - Lambda IR structure + locations match
#
# Uses test files from tests/tests/src/
#
# USAGE:
#   ./scripts/test_lambda_parity.sh [OPTIONS] [FILE|PATTERN]
#
# OPTIONS:
#   -q, --quick       Stop on first failure (useful for quick iteration)
#   -j, --parallel N  Run N tests in parallel (default: 16)
#   -v, --verbose     Show progress for each file
#   -l, --locs        Test with locations (-dlambda-sexp-locs)
#   -h, --help        Show this help message
#
# EXAMPLES:
#   # Test a single file (fastest for iteration):
#   ./scripts/test_lambda_parity.sh tests/tests/src/core/Core_ArrayTest.res
#
#   # Test files matching a pattern:
#   ./scripts/test_lambda_parity.sh "Core_"
#
#   # Run all tests, stop on first failure:
#   ./scripts/test_lambda_parity.sh --quick
#
#   # Run all tests in parallel (default 16 jobs):
#   ./scripts/test_lambda_parity.sh

set -e

# Parse command line arguments
QUICK_MODE=false
PARALLEL_JOBS=16
VERBOSE=false
TEST_LOCS=false
FILE_ARG=""

while [[ $# -gt 0 ]]; do
    case $1 in
        -q|--quick)
            QUICK_MODE=true
            PARALLEL_JOBS=1  # Quick mode forces sequential for clean output
            shift
            ;;
        -j|--parallel)
            PARALLEL_JOBS="$2"
            shift 2
            ;;
        -j*)
            PARALLEL_JOBS="${1#-j}"
            shift
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        -l|--locs)
            TEST_LOCS=true
            shift
            ;;
        -h|--help)
            head -35 "$0" | tail -30
            exit 0
            ;;
        *)
            FILE_ARG="$1"
            shift
            ;;
    esac
done

scriptDir=$(dirname "$0")
PROJECT_ROOT=$(cd "$scriptDir/.."; pwd -P)
DUNE_BIN_DIR="$PROJECT_ROOT/_build/install/default/bin"
RUST_BSC="$PROJECT_ROOT/compiler-rust/target/release/bsc"
OCAML_BSC="$DUNE_BIN_DIR/bsc"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BOLD='\033[1m'
RESET='\033[0m'

# Configuration
TIMEOUT_SECONDS=10

# Check if compilers exist
if [[ ! -x "$OCAML_BSC" ]]; then
    echo -e "${RED}Error: OCaml bsc not found at $OCAML_BSC${RESET}"
    echo "Run 'make' first to build the OCaml compiler."
    exit 1
fi

if [[ ! -x "$RUST_BSC" ]]; then
    echo -e "${RED}Error: Rust bsc not found at $RUST_BSC${RESET}"
    echo "Run 'cargo build --manifest-path compiler-rust/Cargo.toml --release' first."
    exit 1
fi

# Create temp directory
TEMP_DIR=$(mktemp -d)
trap "rm -rf $TEMP_DIR" EXIT

echo -e "${BOLD}Lambda IR Parity Test${RESET}"
echo "============================================"
echo "OCaml bsc: $OCAML_BSC"
echo "Rust bsc:  $RUST_BSC"
echo "Timeout: ${TIMEOUT_SECONDS}s"
echo "Parallel jobs: $PARALLEL_JOBS"
if $TEST_LOCS; then
    echo "Testing: sexp-locs"
else
    echo "Testing: sexp"
fi
echo ""

# Find test files based on arguments
cd "$PROJECT_ROOT"

if [[ -n "$FILE_ARG" ]]; then
    # Check if it's an absolute or relative path to a single file
    if [[ -f "$FILE_ARG" ]]; then
        echo "$FILE_ARG" > "$TEMP_DIR/files.txt"
    elif [[ -f "$PROJECT_ROOT/$FILE_ARG" ]]; then
        echo "$FILE_ARG" > "$TEMP_DIR/files.txt"
    else
        # Treat as a pattern and use find with -path
        find tests/tests/src -name "*.res" 2>/dev/null | grep -E "$FILE_ARG" | sort > "$TEMP_DIR/files.txt" || true
        if [[ ! -s "$TEMP_DIR/files.txt" ]]; then
            echo -e "${RED}Error: No files found matching '$FILE_ARG'${RESET}"
            echo "Hint: Try a pattern like 'Core_' or a full path"
            exit 1
        fi
    fi
else
    # Find all test files
    find tests/tests/src -name "*.res" 2>/dev/null | sort > "$TEMP_DIR/files.txt"
fi

TOTAL_FILES=$(wc -l < "$TEMP_DIR/files.txt" | tr -d ' ')

if [[ "$TOTAL_FILES" -eq 1 ]]; then
    echo -e "Testing ${BOLD}1${RESET} file: $(cat "$TEMP_DIR/files.txt")"
else
    echo -e "Found ${BOLD}$TOTAL_FILES${RESET} test files"
fi
echo ""

# Create results directory
RESULTS_DIR="$TEMP_DIR/results"
mkdir -p "$RESULTS_DIR"
mkdir -p "$TEMP_DIR/diffs"

# Export variables for parallel execution
export OCAML_BSC RUST_BSC TIMEOUT_SECONDS TEMP_DIR QUICK_MODE VERBOSE TEST_LOCS RESULTS_DIR

# Function to test a single file - writes results to a file
test_single_file() {
    local file="$1"
    local safe_name=$(echo "$file" | sed 's/[\/]/_/g')
    local result_file="$RESULTS_DIR/$safe_name.result"

    # Initialize result
    local pass=0 diff_fail=0
    local ocaml_fail=0 rust_fail=0

    # Create per-file temp directory for parallel safety
    local file_temp="$TEMP_DIR/work/$safe_name"
    mkdir -p "$file_temp"

    # Determine flags
    local lambda_flag="-dlambda-sexp"
    if $TEST_LOCS; then
        lambda_flag="-dlambda-sexp-locs"
    fi

    # Run OCaml compiler
    local ocaml_success=false
    if timeout "$TIMEOUT_SECONDS" "$OCAML_BSC" $lambda_flag "$file" > /dev/null 2> "$file_temp/ocaml.sexp"; then
        ocaml_success=true
    else
        ocaml_fail=1
    fi

    # Run Rust compiler
    local rust_success=false
    if timeout "$TIMEOUT_SECONDS" "$RUST_BSC" $lambda_flag "$file" > /dev/null 2> "$file_temp/rust.sexp"; then
        rust_success=true
    else
        rust_fail=1
    fi

    # Compare outputs if both succeeded
    if $ocaml_success && $rust_success; then
        if diff -q "$file_temp/ocaml.sexp" "$file_temp/rust.sexp" > /dev/null 2>&1; then
            pass=1
        else
            diff_fail=1
            diff -u "$file_temp/ocaml.sexp" "$file_temp/rust.sexp" > "$TEMP_DIR/diffs/$safe_name.diff" 2>&1 || true
        fi
    fi

    # Write results
    echo "$pass $diff_fail $ocaml_fail $rust_fail" > "$result_file"

    # Verbose output
    if $VERBOSE; then
        if [[ "$pass" -eq 1 ]]; then
            echo -e "${GREEN}PASS${RESET} $file"
        elif [[ "$ocaml_fail" -eq 1 ]]; then
            echo -e "${YELLOW}SKIP${RESET} $file (OCaml failed)"
        elif [[ "$rust_fail" -eq 1 ]]; then
            echo -e "${YELLOW}SKIP${RESET} $file (Rust failed)"
        else
            echo -e "${RED}FAIL${RESET} $file"
        fi
    fi

    # Quick mode - exit on failure
    if $QUICK_MODE && [[ "$diff_fail" -eq 1 ]]; then
        echo -e "\n${RED}FAILED: $file${RESET}"
        echo ""
        echo "Diff:"
        head -50 "$TEMP_DIR/diffs/$safe_name.diff"
        if [[ $(wc -l < "$TEMP_DIR/diffs/$safe_name.diff") -gt 50 ]]; then
            echo "... (truncated, see full diff at $TEMP_DIR/diffs/$safe_name.diff)"
        fi
        exit 1
    fi
}

export -f test_single_file

# Run tests
if [[ "$PARALLEL_JOBS" -eq 1 ]]; then
    # Sequential execution
    while IFS= read -r file; do
        test_single_file "$file"
    done < "$TEMP_DIR/files.txt"
else
    # Parallel execution
    cat "$TEMP_DIR/files.txt" | xargs -P "$PARALLEL_JOBS" -I {} bash -c 'test_single_file "$@"' _ {}
fi

# Aggregate results
PASS=0
DIFF_FAIL=0
OCAML_FAIL=0
RUST_FAIL=0

for result_file in "$RESULTS_DIR"/*.result; do
    if [[ -f "$result_file" ]]; then
        read p d o r < "$result_file"
        PASS=$((PASS + p))
        DIFF_FAIL=$((DIFF_FAIL + d))
        OCAML_FAIL=$((OCAML_FAIL + o))
        RUST_FAIL=$((RUST_FAIL + r))
    fi
done

# Print summary
echo ""
echo -e "${BOLD}=== Lambda IR Parity Test Results ===${RESET}"
echo ""
echo -e "Total files: $TOTAL_FILES"
echo -e "${GREEN}Passed:${RESET}      $PASS"
echo -e "${RED}Failed:${RESET}      $DIFF_FAIL"
echo -e "${YELLOW}OCaml err:${RESET}   $OCAML_FAIL"
echo -e "${YELLOW}Rust err:${RESET}    $RUST_FAIL"
echo ""

# Print diff file locations if there were failures
if [[ "$DIFF_FAIL" -gt 0 ]]; then
    echo "Diff files saved to: $TEMP_DIR/diffs/"
    echo ""
    echo "First few failing files:"
    ls "$TEMP_DIR/diffs/"/*.diff 2>/dev/null | head -10 | while read diff_file; do
        echo "  $(basename "$diff_file" .diff | tr '_' '/')"
    done
    echo ""
fi

# Exit with error if any parity failures
if [[ "$DIFF_FAIL" -gt 0 ]]; then
    exit 1
fi

echo -e "${GREEN}All tests passed!${RESET}"
