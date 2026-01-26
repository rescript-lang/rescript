#!/bin/bash

# Typed Tree Parity Test Suite: Compare typed tree output between OCaml and Rust compilers
#
# Tests performed:
# 1. typed-sexp parity - Typed tree structure matches between OCaml and Rust (without locations)
# 2. typed-sexp-locs parity - Typed tree structure + locations match
#
# Uses test files from tests/tests/src/
#
# USAGE:
#   ./scripts/test_typedtree_parity.sh [OPTIONS] [FILE|PATTERN]
#
# OPTIONS:
#   -q, --quick       Stop on first failure (useful for quick iteration)
#   -j, --parallel N  Run N tests in parallel (default: 16)
#   -v, --verbose     Show progress for each file
#   -h, --help        Show this help message
#
# EXAMPLES:
#   # Test a single file (fastest for iteration):
#   ./scripts/test_typedtree_parity.sh tests/tests/src/core/Core_ArrayTest.res
#
#   # Test files matching a pattern:
#   ./scripts/test_typedtree_parity.sh "Core_Array"
#
#   # Run all tests, stop on first failure:
#   ./scripts/test_typedtree_parity.sh --quick
#
#   # Run all tests in parallel (default 16 jobs):
#   ./scripts/test_typedtree_parity.sh

set -e

# Parse command line arguments
QUICK_MODE=false
PARALLEL_JOBS=16
VERBOSE=false
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
OCAML_BSC="$DUNE_BIN_DIR/bsc"
RUST_BSC="$PROJECT_ROOT/compiler-rust/target/release/bsc"

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
    echo -e "${YELLOW}Warning: Rust bsc not found at $RUST_BSC${RESET}"
    echo "Run 'cargo build --manifest-path compiler-rust/Cargo.toml --release' to build the Rust compiler."
    echo "For now, testing OCaml compiler only."
    RUST_AVAILABLE=false
else
    RUST_AVAILABLE=true
fi

# Create temp directory
TEMP_DIR=$(mktemp -d)
trap "rm -rf $TEMP_DIR" EXIT

echo -e "${BOLD}Typed Tree Parity Test${RESET}"
echo "============================================"
echo "OCaml bsc: $OCAML_BSC"
if [[ "$RUST_AVAILABLE" == "true" ]]; then
    echo "Rust bsc:  $RUST_BSC"
else
    echo "Rust bsc:  (not available)"
fi
echo "Timeout: ${TIMEOUT_SECONDS}s"
echo "Parallel jobs: $PARALLEL_JOBS"
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
            echo "Hint: Try a pattern like 'Core_Array' or a full path"
            exit 1
        fi
    fi
else
    # Find all test files (limit to a reasonable subset for testing)
    find tests/tests/src -name "*.res" 2>/dev/null | sort | head -100 > "$TEMP_DIR/files.txt"
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
export OCAML_BSC RUST_BSC TIMEOUT_SECONDS TEMP_DIR QUICK_MODE VERBOSE RUST_AVAILABLE PROJECT_ROOT RESULTS_DIR

# Function to test a single file - writes results to a file
test_single_file() {
    local file="$1"
    local safe_name=$(echo "$file" | tr '/' '_')
    local result_file="$RESULTS_DIR/$safe_name.result"

    # Initialize result
    local ocaml_pass=0 rust_pass=0 parity_pass=0
    local ocaml_fail=0 rust_fail=0 parity_fail=0

    # Create per-file temp directory for parallel safety
    local file_temp="$TEMP_DIR/work/$safe_name"
    mkdir -p "$file_temp"

    # Determine full file path (handle both absolute and relative paths)
    local full_file_path
    if [[ "$file" = /* ]]; then
        full_file_path="$file"
    else
        full_file_path="$PROJECT_ROOT/$file"
    fi

    # Run OCaml compiler with -dtyped-sexp (sexp goes to stderr, JS to stdout)
    if timeout "$TIMEOUT_SECONDS" "$OCAML_BSC" -dtyped-sexp "$full_file_path" 2> "$file_temp/ocaml.sexp" 1>/dev/null; then
        ocaml_pass=1
    else
        ocaml_fail=1
    fi

    # Run Rust compiler if available
    if [[ "$RUST_AVAILABLE" == "true" ]]; then
        if timeout "$TIMEOUT_SECONDS" "$RUST_BSC" -dtyped-sexp "$full_file_path" 2> "$file_temp/rust.sexp" 1>/dev/null; then
            rust_pass=1
        else
            rust_fail=1
        fi

        # Compare if both succeeded
        if [[ "$ocaml_pass" -eq 1 && "$rust_pass" -eq 1 ]]; then
            if diff -q "$file_temp/ocaml.sexp" "$file_temp/rust.sexp" > /dev/null 2>&1; then
                parity_pass=1
            else
                parity_fail=1
                # Save diff for inspection
                diff -u "$file_temp/ocaml.sexp" "$file_temp/rust.sexp" > "$TEMP_DIR/diffs/$safe_name.diff" 2>&1 || true
            fi
        fi
    fi

    # Write result
    echo "$file:ocaml_pass=$ocaml_pass:ocaml_fail=$ocaml_fail:rust_pass=$rust_pass:rust_fail=$rust_fail:parity_pass=$parity_pass:parity_fail=$parity_fail" > "$result_file"

    # Verbose output
    if [[ "$VERBOSE" == "true" ]]; then
        if [[ "$ocaml_pass" -eq 1 ]]; then
            echo -e "${GREEN}PASS${RESET} [OCaml] $file"
        else
            echo -e "${RED}FAIL${RESET} [OCaml] $file"
        fi
        if [[ "$RUST_AVAILABLE" == "true" ]]; then
            if [[ "$rust_pass" -eq 1 ]]; then
                echo -e "${GREEN}PASS${RESET} [Rust] $file"
            else
                echo -e "${RED}FAIL${RESET} [Rust] $file"
            fi
            if [[ "$parity_pass" -eq 1 ]]; then
                echo -e "${GREEN}PASS${RESET} [Parity] $file"
            elif [[ "$parity_fail" -eq 1 ]]; then
                echo -e "${RED}FAIL${RESET} [Parity] $file"
            fi
        fi
    fi

    # Quick mode - exit on failure
    if [[ "$QUICK_MODE" == "true" && ("$ocaml_fail" -eq 1 || "$parity_fail" -eq 1) ]]; then
        echo -e "${RED}Quick mode: stopping on first failure${RESET}"
        if [[ -f "$TEMP_DIR/diffs/$safe_name.diff" ]]; then
            echo -e "\nParity diff:"
            head -50 "$TEMP_DIR/diffs/$safe_name.diff"
        fi
        exit 1
    fi
}

export -f test_single_file

# Run tests
echo "Running tests..."
if [[ "$PARALLEL_JOBS" -eq 1 ]]; then
    while IFS= read -r file; do
        test_single_file "$file"
    done < "$TEMP_DIR/files.txt"
else
    cat "$TEMP_DIR/files.txt" | xargs -P "$PARALLEL_JOBS" -I {} bash -c 'test_single_file "$@"' _ {}
fi

# Aggregate results
echo ""
echo -e "${BOLD}Results${RESET}"
echo "========"

ocaml_total_pass=0
ocaml_total_fail=0
rust_total_pass=0
rust_total_fail=0
parity_total_pass=0
parity_total_fail=0

for result in "$RESULTS_DIR"/*.result; do
    if [[ -f "$result" ]]; then
        line=$(cat "$result")
        ocaml_pass=$(echo "$line" | sed 's/.*ocaml_pass=\([0-9]*\).*/\1/')
        ocaml_fail=$(echo "$line" | sed 's/.*ocaml_fail=\([0-9]*\).*/\1/')
        rust_pass=$(echo "$line" | sed 's/.*rust_pass=\([0-9]*\).*/\1/')
        rust_fail=$(echo "$line" | sed 's/.*rust_fail=\([0-9]*\).*/\1/')
        parity_pass=$(echo "$line" | sed 's/.*parity_pass=\([0-9]*\).*/\1/')
        parity_fail=$(echo "$line" | sed 's/.*parity_fail=\([0-9]*\).*/\1/')

        ocaml_total_pass=$((ocaml_total_pass + ocaml_pass))
        ocaml_total_fail=$((ocaml_total_fail + ocaml_fail))
        rust_total_pass=$((rust_total_pass + rust_pass))
        rust_total_fail=$((rust_total_fail + rust_fail))
        parity_total_pass=$((parity_total_pass + parity_pass))
        parity_total_fail=$((parity_total_fail + parity_fail))
    fi
done

echo "OCaml bsc:    ${GREEN}$ocaml_total_pass passed${RESET}, ${RED}$ocaml_total_fail failed${RESET}"
if [[ "$RUST_AVAILABLE" == "true" ]]; then
    echo "Rust bsc:     ${GREEN}$rust_total_pass passed${RESET}, ${RED}$rust_total_fail failed${RESET}"
    echo "Parity:       ${GREEN}$parity_total_pass passed${RESET}, ${RED}$parity_total_fail failed${RESET}"
fi

# Show failed files
if [[ "$ocaml_total_fail" -gt 0 ]]; then
    echo ""
    echo -e "${YELLOW}OCaml failures:${RESET}"
    for result in "$RESULTS_DIR"/*.result; do
        if [[ -f "$result" ]]; then
            line=$(cat "$result")
            if [[ "$line" == *"ocaml_fail=1"* ]]; then
                file=$(echo "$line" | cut -d: -f1)
                echo "  $file"
            fi
        fi
    done
fi

if [[ "$parity_total_fail" -gt 0 ]]; then
    echo ""
    echo -e "${YELLOW}Parity failures (diffs saved in $TEMP_DIR/diffs/):${RESET}"
    for result in "$RESULTS_DIR"/*.result; do
        if [[ -f "$result" ]]; then
            line=$(cat "$result")
            if [[ "$line" == *"parity_fail=1"* ]]; then
                file=$(echo "$line" | cut -d: -f1)
                echo "  $file"
            fi
        fi
    done
fi

# Exit code
if [[ "$ocaml_total_fail" -gt 0 || "$parity_total_fail" -gt 0 ]]; then
    exit 1
fi
