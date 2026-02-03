#!/bin/bash

# Rust Compiler Parity Test Suite
# Tests the Rust compiler against the OCaml reference implementation
# Reports which tests failed and overall percentage parity

set -e

scriptDir=$(dirname "$0")
PROJECT_ROOT=$(cd "$scriptDir/.."; pwd -P)

# Directories
DUNE_BIN_DIR="$PROJECT_ROOT/_build/install/default/bin"
RUST_DIR="$PROJECT_ROOT/compiler-rust"
RUST_TARGET="${RUST_TARGET:-release}"
RUST_BSC="$RUST_DIR/target/$RUST_TARGET/bsc"
RUST_PARSER="$RUST_DIR/target/$RUST_TARGET/res_parser_rust"
OCAML_BSC="$PROJECT_ROOT/packages/@rescript/$(./scripts/platform.sh)/bin/bsc.exe"
OCAML_PARSER="$DUNE_BIN_DIR/res_parser"

# Output directories
TEMP_DIR="$PROJECT_ROOT/temp/rust-parity"
DIFF_DIR="$TEMP_DIR/diffs"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BOLD='\033[1m'
DIM='\033[2m'
RESET='\033[0m'

# Configuration
TIMEOUT_SECONDS=10
SHOW_MAX_FAILURES=10

# Counters for overall summary
total_tests=0
total_passed=0
total_failed=0

# Parse arguments
SKIP_PARSER_AST=false
SKIP_SYNTAX=false
SKIP_JS=false
VERBOSE=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --skip-parser-ast)
            SKIP_PARSER_AST=true
            shift
            ;;
        --skip-syntax)
            SKIP_SYNTAX=true
            shift
            ;;
        --skip-js)
            SKIP_JS=true
            shift
            ;;
        --verbose|-v)
            VERBOSE=true
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [options]"
            echo ""
            echo "Options:"
            echo "  --skip-parser-ast  Skip parser AST parity tests"
            echo "  --skip-syntax      Skip syntax tests"
            echo "  --skip-js          Skip JS compilation snapshot tests"
            echo "  --verbose, -v      Show more detailed output"
            echo "  --help, -h         Show this help message"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Utility functions
print_header() {
    echo ""
    echo -e "${BOLD}$1${RESET}"
    echo "----------------------------------------------------------------------"
}

print_section_header() {
    echo ""
    echo -e "${BOLD}══════════════════════════════════════════════════════════════════════${RESET}"
    echo -e "${BOLD}$1${RESET}"
    echo -e "${BOLD}══════════════════════════════════════════════════════════════════════${RESET}"
}

# Check prerequisites
check_prerequisites() {
    local missing=false

    if [[ ! -x "$OCAML_BSC" ]]; then
        echo -e "${RED}Error: OCaml bsc not found at $OCAML_BSC${RESET}"
        echo "Run 'make' first to build the OCaml compiler."
        missing=true
    fi

    if [[ ! -x "$RUST_BSC" ]]; then
        echo -e "${RED}Error: Rust bsc not found at $RUST_BSC${RESET}"
        echo "Run 'cargo build --manifest-path compiler-rust/Cargo.toml --release' first."
        missing=true
    fi

    if [[ ! -x "$RUST_PARSER" ]]; then
        echo -e "${RED}Error: Rust parser not found at $RUST_PARSER${RESET}"
        echo "Run 'cargo build --manifest-path compiler-rust/Cargo.toml --release' first."
        missing=true
    fi

    if [[ ! -x "$OCAML_PARSER" ]]; then
        echo -e "${RED}Error: OCaml parser not found at $OCAML_PARSER${RESET}"
        echo "Run 'make' first to build the OCaml parser."
        missing=true
    fi

    if $missing; then
        exit 2
    fi
}

# Setup temp directory
setup_temp_dir() {
    rm -rf "$TEMP_DIR"
    mkdir -p "$TEMP_DIR"
    mkdir -p "$DIFF_DIR"
}

# =============================================================================
# TEST 1: Parser AST Parity
# =============================================================================
run_parser_ast_tests() {
    if $SKIP_PARSER_AST; then
        echo -e "${DIM}Skipping parser AST parity tests${RESET}"
        return
    fi

    print_header "PARSER AST PARITY"

    local pass_count=0
    local fail_count=0
    local ocaml_fail_count=0
    local rust_fail_count=0
    local diff_count=0
    local failed_files=""

    cd "$PROJECT_ROOT/tests"

    # Find all test files from idempotency and printer directories
    find syntax_tests/data/idempotency syntax_tests/data/printer -name "*.res" -o -name "*.resi" 2>/dev/null | sort > "$TEMP_DIR/parser_files.txt"
    local file_count=$(wc -l < "$TEMP_DIR/parser_files.txt" | tr -d ' ')

    echo "Testing $file_count files..."

    local file_index=0
    while read -r file; do
        file_index=$((file_index + 1))

        # Progress indicator
        if [[ $((file_index % 100)) -eq 0 ]] && $VERBOSE; then
            echo "  Progress: $file_index / $file_count files..."
        fi

        # Determine if it's an interface file
        local intf_flag=""
        case "$file" in
            *.resi)
                intf_flag="-interface"
                ;;
        esac

        # Run OCaml parser
        local ocaml_output="$TEMP_DIR/ocaml.sexp"
        local ocaml_success=true
        if ! timeout "$TIMEOUT_SECONDS" "$OCAML_PARSER" $intf_flag -print sexp "$file" > "$ocaml_output" 2>/dev/null; then
            ocaml_success=false
            ocaml_fail_count=$((ocaml_fail_count + 1))
        fi

        # Run Rust parser
        local rust_output="$TEMP_DIR/rust.sexp"
        local rust_success=true
        if ! timeout "$TIMEOUT_SECONDS" "$RUST_PARSER" $intf_flag -print sexp "$file" > "$rust_output" 2>/dev/null; then
            rust_success=false
            rust_fail_count=$((rust_fail_count + 1))
            failed_files="$failed_files$file (Rust parse error)\n"
        fi

        # Compare outputs if both succeeded
        if $ocaml_success && $rust_success; then
            if diff -q "$ocaml_output" "$rust_output" > /dev/null 2>&1; then
                pass_count=$((pass_count + 1))
            else
                diff_count=$((diff_count + 1))
                failed_files="$failed_files$file (AST differs)\n"

                # Save diff for inspection
                local safe_name=$(echo "$file" | tr '/' '_')
                diff -u "$ocaml_output" "$rust_output" > "$DIFF_DIR/parser_$safe_name.diff" 2>&1 || true
            fi
        fi

    done < "$TEMP_DIR/parser_files.txt"

    cd "$PROJECT_ROOT"

    # Calculate totals
    local comparable_count=$((pass_count + diff_count))
    fail_count=$((rust_fail_count + diff_count))

    # Report results
    if [[ $comparable_count -gt 0 ]]; then
        local pct=$(python3 -c "print(f'{100 * $pass_count / $comparable_count:.1f}')")
        echo -e "Passed: ${GREEN}$pass_count${RESET}/$comparable_count (${pct}%)"
    else
        echo -e "Passed: ${YELLOW}0/0${RESET} (no comparable files)"
    fi

    if [[ $rust_fail_count -gt 0 ]]; then
        echo -e "Rust parse failures: ${YELLOW}$rust_fail_count${RESET}"
    fi

    if [[ $diff_count -gt 0 ]]; then
        echo -e "AST differences: ${RED}$diff_count${RESET}"
    fi

    # Show failed files
    if [[ -n "$failed_files" ]]; then
        echo ""
        echo -e "${BOLD}Failed (first $SHOW_MAX_FAILURES):${RESET}"
        echo -e "$failed_files" | grep -v "^$" | head -$SHOW_MAX_FAILURES | while read -r line; do
            echo "  - $line"
        done
        local total_failures=$(echo -e "$failed_files" | grep -v "^$" | wc -l | tr -d ' ')
        if [[ $total_failures -gt $SHOW_MAX_FAILURES ]]; then
            echo "  ... and $((total_failures - SHOW_MAX_FAILURES)) more"
        fi
    fi

    # Update global counters
    total_tests=$((total_tests + comparable_count))
    total_passed=$((total_passed + pass_count))
    total_failed=$((total_failed + diff_count))
}

# =============================================================================
# TEST 2: Syntax Tests
# =============================================================================
run_syntax_tests() {
    if $SKIP_SYNTAX; then
        echo -e "${DIM}Skipping syntax tests${RESET}"
        return
    fi

    print_header "SYNTAX TESTS"

    local pass_count=0
    local fail_count=0
    local failed_files=""

    cd "$PROJECT_ROOT/tests"
    mkdir -p "$TEMP_DIR/syntax"

    # Function to get expected path
    exp() {
        echo "$(dirname $1)/expected/$(basename $1).txt"
    }

    # Collect all syntax test files
    local all_files=""

    # Parsing tests with recovery
    find syntax_tests/data/parsing/{errors,infiniteLoops,recovery} -name "*.res" -o -name "*.resi" 2>/dev/null | sort > "$TEMP_DIR/syntax_parsing_recovery.txt"

    # Parsing tests without recovery
    find syntax_tests/data/parsing/{grammar,other} -name "*.res" -o -name "*.resi" 2>/dev/null | sort > "$TEMP_DIR/syntax_parsing_grammar.txt"

    # Printing tests
    find syntax_tests/data/{printer,conversion} -name "*.res" -o -name "*.resi" -o -name "*.ml" -o -name "*.mli" 2>/dev/null | sort > "$TEMP_DIR/syntax_printer.txt"

    # AST mapping tests
    find syntax_tests/data/ast-mapping -name "*.res" -o -name "*.resi" -o -name "*.ml" -o -name "*.mli" 2>/dev/null | sort > "$TEMP_DIR/syntax_ast_mapping.txt"

    # PPX tests
    find syntax_tests/data/ppx/react -name "*.res" -o -name "*.resi" 2>/dev/null | sort > "$TEMP_DIR/syntax_ppx.txt"

    local file_count=$(cat "$TEMP_DIR"/syntax_*.txt | wc -l | tr -d ' ')
    echo "Testing $file_count files..."

    # Process parsing tests with recovery
    while read -r file; do
        local expected=$(exp "$file")
        local actual="$TEMP_DIR/syntax/$(echo "$file" | tr '/' '_').txt"

        if timeout "$TIMEOUT_SECONDS" "$RUST_PARSER" -recover -print ml "$file" > "$actual" 2>&1; then
            if diff -q "$expected" "$actual" > /dev/null 2>&1; then
                pass_count=$((pass_count + 1))
            else
                fail_count=$((fail_count + 1))
                failed_files="$failed_files$file (output differs)\n"
                diff -u "$expected" "$actual" > "$DIFF_DIR/syntax_$(echo "$file" | tr '/' '_').diff" 2>&1 || true
            fi
        else
            fail_count=$((fail_count + 1))
            failed_files="$failed_files$file (parse error/timeout)\n"
        fi
    done < "$TEMP_DIR/syntax_parsing_recovery.txt"

    # Process parsing tests without recovery
    while read -r file; do
        local expected=$(exp "$file")
        local actual="$TEMP_DIR/syntax/$(echo "$file" | tr '/' '_').txt"

        # Run parser (may exit non-zero for files with syntax errors, which is expected)
        # Capture exit code without triggering set -e
        local exit_code=0
        timeout "$TIMEOUT_SECONDS" "$RUST_PARSER" -print ml "$file" > "$actual" 2>&1 || exit_code=$?

        if [[ $exit_code -eq 124 ]]; then
            # Timeout
            fail_count=$((fail_count + 1))
            failed_files="$failed_files$file (timeout)\n"
            continue
        fi
        # Compare output regardless of parser exit code (files with syntax errors exit non-zero)
        if diff -q "$expected" "$actual" > /dev/null 2>&1; then
            pass_count=$((pass_count + 1))
        else
            fail_count=$((fail_count + 1))
            failed_files="$failed_files$file (output differs)\n"
            diff -u "$expected" "$actual" > "$DIFF_DIR/syntax_$(echo "$file" | tr '/' '_').diff" 2>&1 || true
        fi
    done < "$TEMP_DIR/syntax_parsing_grammar.txt"

    # Process printing tests
    while read -r file; do
        local expected=$(exp "$file")
        local actual="$TEMP_DIR/syntax/$(echo "$file" | tr '/' '_').txt"

        if timeout "$TIMEOUT_SECONDS" "$RUST_PARSER" "$file" > "$actual" 2>&1; then
            if diff -q "$expected" "$actual" > /dev/null 2>&1; then
                pass_count=$((pass_count + 1))
            else
                fail_count=$((fail_count + 1))
                failed_files="$failed_files$file (output differs)\n"
                diff -u "$expected" "$actual" > "$DIFF_DIR/syntax_$(echo "$file" | tr '/' '_').diff" 2>&1 || true
            fi
        else
            fail_count=$((fail_count + 1))
            failed_files="$failed_files$file (parse error/timeout)\n"
        fi
    done < "$TEMP_DIR/syntax_printer.txt"

    # Process AST mapping tests
    while read -r file; do
        local expected=$(exp "$file")
        local actual="$TEMP_DIR/syntax/$(echo "$file" | tr '/' '_').txt"

        if timeout "$TIMEOUT_SECONDS" "$RUST_PARSER" -test-ast-conversion -jsx-version 4 "$file" > "$actual" 2>&1; then
            if diff -q "$expected" "$actual" > /dev/null 2>&1; then
                pass_count=$((pass_count + 1))
            else
                fail_count=$((fail_count + 1))
                failed_files="$failed_files$file (output differs)\n"
                diff -u "$expected" "$actual" > "$DIFF_DIR/syntax_$(echo "$file" | tr '/' '_').diff" 2>&1 || true
            fi
        else
            fail_count=$((fail_count + 1))
            failed_files="$failed_files$file (parse error/timeout)\n"
        fi
    done < "$TEMP_DIR/syntax_ast_mapping.txt"

    # Process PPX tests
    while read -r file; do
        local expected=$(exp "$file")
        local actual="$TEMP_DIR/syntax/$(echo "$file" | tr '/' '_').txt"

        if timeout "$TIMEOUT_SECONDS" "$RUST_PARSER" -jsx-version 4 "$file" > "$actual" 2>&1; then
            if diff -q "$expected" "$actual" > /dev/null 2>&1; then
                pass_count=$((pass_count + 1))
            else
                fail_count=$((fail_count + 1))
                failed_files="$failed_files$file (output differs)\n"
                diff -u "$expected" "$actual" > "$DIFF_DIR/syntax_$(echo "$file" | tr '/' '_').diff" 2>&1 || true
            fi
        else
            fail_count=$((fail_count + 1))
            failed_files="$failed_files$file (parse error/timeout)\n"
        fi
    done < "$TEMP_DIR/syntax_ppx.txt"

    cd "$PROJECT_ROOT"

    # Report results
    local total=$((pass_count + fail_count))
    if [[ $total -gt 0 ]]; then
        local pct=$(python3 -c "print(f'{100 * $pass_count / $total:.1f}')")
        echo -e "Passed: ${GREEN}$pass_count${RESET}/$total (${pct}%)"
    else
        echo -e "Passed: ${YELLOW}0/0${RESET} (no files)"
    fi

    if [[ $fail_count -gt 0 ]]; then
        echo -e "Failed: ${RED}$fail_count${RESET}"
    fi

    # Show failed files
    if [[ -n "$failed_files" ]]; then
        echo ""
        echo -e "${BOLD}Failed (first $SHOW_MAX_FAILURES):${RESET}"
        echo -e "$failed_files" | grep -v "^$" | head -$SHOW_MAX_FAILURES | while read -r line; do
            echo "  - $line"
        done
        local total_failures=$(echo -e "$failed_files" | grep -v "^$" | wc -l | tr -d ' ')
        if [[ $total_failures -gt $SHOW_MAX_FAILURES ]]; then
            echo "  ... and $((total_failures - SHOW_MAX_FAILURES)) more"
        fi
    fi

    # Update global counters
    total_tests=$((total_tests + total))
    total_passed=$((total_passed + pass_count))
    total_failed=$((total_failed + fail_count))
}

# =============================================================================
# TEST 3: JS Compilation Snapshots
# =============================================================================
run_js_compilation_tests() {
    if $SKIP_JS; then
        echo -e "${DIM}Skipping JS compilation snapshot tests${RESET}"
        return
    fi

    print_header "JS COMPILATION SNAPSHOTS"

    local pass_count=0
    local compile_error_count=0
    local diff_count=0
    local failed_files=""

    cd "$PROJECT_ROOT/tests/tests"
    mkdir -p "$TEMP_DIR/js"

    # Find all .res files that have corresponding .mjs snapshots
    find src -name "*.res" 2>/dev/null | sort > "$TEMP_DIR/js_files.txt"
    local file_count=$(wc -l < "$TEMP_DIR/js_files.txt" | tr -d ' ')

    echo "Testing $file_count files..."

    local file_index=0
    while read -r res_file; do
        file_index=$((file_index + 1))

        # Progress indicator
        if [[ $((file_index % 50)) -eq 0 ]] && $VERBOSE; then
            echo "  Progress: $file_index / $file_count files..."
        fi

        # Get the expected .mjs file path
        local mjs_file="${res_file%.res}.mjs"

        # Skip if no snapshot exists (some files might be dependencies only)
        if [[ ! -f "$mjs_file" ]]; then
            continue
        fi

        # Create output directory
        local output_dir="$TEMP_DIR/js/$(dirname "$res_file")"
        mkdir -p "$output_dir"

        # Compile with Rust bsc
        local actual_mjs="$output_dir/$(basename "${res_file%.res}.mjs")"
        local compile_output="$TEMP_DIR/js/compile_output.txt"

        # Note: We compile to a temp location to not disturb the actual test files
        # The Rust bsc should output to stdout or we need to copy the file
        if timeout "$TIMEOUT_SECONDS" "$RUST_BSC" -bs-package-name rescript-tests -bs-module-type esmodule "$res_file" -o "$actual_mjs" > "$compile_output" 2>&1; then
            # Check if output file was created
            if [[ -f "$actual_mjs" ]]; then
                if diff -q "$mjs_file" "$actual_mjs" > /dev/null 2>&1; then
                    pass_count=$((pass_count + 1))
                else
                    diff_count=$((diff_count + 1))
                    failed_files="$failed_files$res_file (output differs)\n"
                    diff -u "$mjs_file" "$actual_mjs" > "$DIFF_DIR/js_$(echo "$res_file" | tr '/' '_').diff" 2>&1 || true
                fi
            else
                compile_error_count=$((compile_error_count + 1))
                failed_files="$failed_files$res_file (no output file)\n"
                cp "$compile_output" "$DIFF_DIR/js_$(echo "$res_file" | tr '/' '_').err" 2>/dev/null || true
            fi
        else
            compile_error_count=$((compile_error_count + 1))
            failed_files="$failed_files$res_file (compile error)\n"
            cp "$compile_output" "$DIFF_DIR/js_$(echo "$res_file" | tr '/' '_').err" 2>/dev/null || true
        fi

    done < "$TEMP_DIR/js_files.txt"

    cd "$PROJECT_ROOT"

    # Report results
    local total=$((pass_count + compile_error_count + diff_count))
    if [[ $total -gt 0 ]]; then
        local pct=$(python3 -c "print(f'{100 * $pass_count / $total:.1f}')")
        echo -e "Passed: ${GREEN}$pass_count${RESET}/$total (${pct}%)"
    else
        echo -e "Passed: ${YELLOW}0/0${RESET} (no files with snapshots)"
    fi

    if [[ $compile_error_count -gt 0 ]]; then
        echo -e "Compile errors: ${RED}$compile_error_count${RESET}"
    fi

    if [[ $diff_count -gt 0 ]]; then
        echo -e "Output differs: ${YELLOW}$diff_count${RESET}"
    fi

    # Show failed files
    if [[ -n "$failed_files" ]]; then
        echo ""
        echo -e "${BOLD}Failed (first $SHOW_MAX_FAILURES):${RESET}"
        echo -e "$failed_files" | grep -v "^$" | head -$SHOW_MAX_FAILURES | while read -r line; do
            echo "  - $line"
        done
        local total_failures=$(echo -e "$failed_files" | grep -v "^$" | wc -l | tr -d ' ')
        if [[ $total_failures -gt $SHOW_MAX_FAILURES ]]; then
            echo "  ... and $((total_failures - SHOW_MAX_FAILURES)) more"
        fi
    fi

    # Update global counters
    total_tests=$((total_tests + total))
    total_passed=$((total_passed + pass_count))
    total_failed=$((total_failed + compile_error_count + diff_count))
}

# =============================================================================
# Main
# =============================================================================

print_section_header "RUST COMPILER PARITY REPORT"
echo ""
echo "OCaml bsc:    $OCAML_BSC"
echo "Rust bsc:     $RUST_BSC"
echo "Rust parser:  $RUST_PARSER"
echo "OCaml parser: $OCAML_PARSER"
echo "Timeout:      ${TIMEOUT_SECONDS}s per file"
echo ""

check_prerequisites
setup_temp_dir

run_parser_ast_tests
run_syntax_tests
run_js_compilation_tests

# Final summary
print_section_header "OVERALL SUMMARY"
echo ""

if [[ $total_tests -gt 0 ]]; then
    pct=$(python3 -c "print(f'{100 * $total_passed / $total_tests:.1f}')")
    echo -e "${BOLD}Total:${RESET} $total_passed / $total_tests tests passed (${pct}% parity)"
else
    echo -e "${YELLOW}No tests were run${RESET}"
fi

echo ""
echo "Diff files saved to: $DIFF_DIR"
echo ""

# Exit code
if [[ $total_failed -eq 0 && $total_passed -gt 0 ]]; then
    echo -e "${GREEN}${BOLD}SUCCESS: 100% parity achieved!${RESET}"
    exit 0
elif [[ $total_passed -eq 0 ]]; then
    echo -e "${RED}${BOLD}ERROR: No tests passed${RESET}"
    exit 2
else
    echo -e "${YELLOW}${BOLD}INCOMPLETE: $total_failed tests failed${RESET}"
    exit 1
fi
