#!/bin/bash

# AST Parity Test: Ensures Rust parser produces identical AST to OCaml parser
# Uses roundtrip test files from tests/syntax_tests/data/{idempotency,printer}

set -e

scriptDir=$(dirname "$0")
PROJECT_ROOT=$(cd "$scriptDir/.."; pwd -P)
DUNE_BIN_DIR="$PROJECT_ROOT/_build/install/default/bin"
RUST_PARSER="$PROJECT_ROOT/compiler-rust/target/release/res_parser_rust"
OCAML_PARSER="$DUNE_BIN_DIR/res_parser"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BOLD='\033[1m'
RESET='\033[0m'

# Configuration
TIMEOUT_SECONDS=5

# Check if parsers exist
if [[ ! -x "$OCAML_PARSER" ]]; then
    echo -e "${RED}Error: OCaml parser not found at $OCAML_PARSER${RESET}"
    echo "Run 'make' first to build the OCaml parser."
    exit 1
fi

if [[ ! -x "$RUST_PARSER" ]]; then
    echo -e "${RED}Error: Rust parser not found at $RUST_PARSER${RESET}"
    echo "Run 'cargo build --manifest-path compiler-rust/Cargo.toml --release' first."
    exit 1
fi

# Create temp directory
TEMP_DIR=$(mktemp -d)
trap "rm -rf $TEMP_DIR" EXIT

echo -e "${BOLD}Parser AST Parity Test${RESET}"
echo "============================================"
echo "OCaml parser: $OCAML_PARSER"
echo "Rust parser:  $RUST_PARSER"
echo "Timeout: ${TIMEOUT_SECONDS}s"
echo ""

# Find all test files
cd "$PROJECT_ROOT/tests"
find syntax_tests/data/idempotency syntax_tests/data/printer -name "*.res" -o -name "*.resi" 2>/dev/null | sort > "$TEMP_DIR/files.txt"
TOTAL_FILES=$(wc -l < "$TEMP_DIR/files.txt" | tr -d ' ')

echo -e "Found ${BOLD}$TOTAL_FILES${RESET} test files"
echo ""

# Counters
file_index=0
pass_count=0
fail_count=0
ocaml_fail_count=0
rust_fail_count=0
diff_count=0

# Store failures for reporting
diff_files=""
rust_fail_files=""
ocaml_fail_files=""

echo -e "${BOLD}Running AST parity tests...${RESET}"
echo ""

while read -r file; do
    file_index=$((file_index + 1))

    # Progress indicator
    if [[ $((file_index % 100)) -eq 0 ]]; then
        echo "  Progress: $file_index / $TOTAL_FILES files..."
    fi

    # Determine if it's an interface file
    intf_flag_ocaml=""
    intf_flag_rust=""
    case "$file" in
        *.resi)
            intf_flag_ocaml="-interface"
            intf_flag_rust="--interface"
            ;;
    esac

    # Run OCaml parser
    ocaml_output="$TEMP_DIR/ocaml.sexp"
    ocaml_err="$TEMP_DIR/ocaml.err"
    if timeout "$TIMEOUT_SECONDS" "$OCAML_PARSER" $intf_flag_ocaml -print sexp "$file" > "$ocaml_output" 2> "$ocaml_err"; then
        ocaml_success=true
    else
        ocaml_success=false
        ocaml_fail_count=$((ocaml_fail_count + 1))
        ocaml_fail_files="$ocaml_fail_files$file\n"
    fi

    # Run Rust parser
    rust_output="$TEMP_DIR/rust.sexp"
    rust_err="$TEMP_DIR/rust.err"
    if timeout "$TIMEOUT_SECONDS" "$RUST_PARSER" $intf_flag_rust --print sexp "$file" > "$rust_output" 2> "$rust_err"; then
        rust_success=true
    else
        rust_success=false
        rust_fail_count=$((rust_fail_count + 1))
        rust_fail_files="$rust_fail_files$file\n"
    fi

    # Compare outputs if both succeeded
    if $ocaml_success && $rust_success; then
        # Exact comparison - no normalization
        if diff -q "$ocaml_output" "$rust_output" > /dev/null 2>&1; then
            pass_count=$((pass_count + 1))
        else
            diff_count=$((diff_count + 1))
            diff_files="$diff_files$file\n"

            # Save diff for later inspection
            diff_dir="$TEMP_DIR/diffs"
            mkdir -p "$diff_dir"
            safe_name=$(echo "$file" | tr '/' '_')
            diff -u "$ocaml_output" "$rust_output" > "$diff_dir/$safe_name.diff" 2>&1 || true
        fi
    fi

done < "$TEMP_DIR/files.txt"

# Calculate totals
fail_count=$((ocaml_fail_count + rust_fail_count + diff_count))

echo ""
echo ""
echo -e "${BOLD}============================================${RESET}"
echo -e "${BOLD}RESULTS${RESET}"
echo -e "${BOLD}============================================${RESET}"
echo ""

echo -e "${BOLD}Summary:${RESET}"
echo "  Total files tested:     $TOTAL_FILES"
echo "  AST matches (PASS):     $pass_count"
echo "  AST mismatches (DIFF):  $diff_count"
echo "  OCaml parse failures:   $ocaml_fail_count"
echo "  Rust parse failures:    $rust_fail_count"
echo ""

# Show AST mismatches
if [[ $diff_count -gt 0 ]]; then
    echo -e "${BOLD}${RED}Files with AST differences ($diff_count):${RESET}"
    echo -e "$diff_files" | grep -v "^$" | head -30 | while read file; do
        echo "  $file"
    done
    if [[ $diff_count -gt 30 ]]; then
        remaining=$((diff_count - 30))
        echo "  ... and $remaining more"
    fi
    echo ""

    # Show first diff as example
    echo -e "${BOLD}Example diff (first file):${RESET}"
    first_diff=$(ls "$TEMP_DIR/diffs/"*.diff 2>/dev/null | head -1)
    if [[ -n "$first_diff" ]]; then
        head -50 "$first_diff"
        lines=$(wc -l < "$first_diff")
        if [[ $lines -gt 50 ]]; then
            echo "  ... (diff truncated, $lines total lines)"
        fi
    fi
    echo ""
fi

# Show Rust failures (if not too many)
if [[ $rust_fail_count -gt 0 && $rust_fail_count -le 50 ]]; then
    echo -e "${BOLD}${YELLOW}Files where Rust parser failed ($rust_fail_count):${RESET}"
    echo -e "$rust_fail_files" | grep -v "^$" | head -20 | while read file; do
        echo "  $file"
    done
    if [[ $rust_fail_count -gt 20 ]]; then
        remaining=$((rust_fail_count - 20))
        echo "  ... and $remaining more"
    fi
    echo ""
elif [[ $rust_fail_count -gt 50 ]]; then
    echo -e "${BOLD}${YELLOW}Rust parser failed on $rust_fail_count files (too many to list)${RESET}"
    echo ""
fi

# Show OCaml failures (if any)
if [[ $ocaml_fail_count -gt 0 ]]; then
    echo -e "${BOLD}${YELLOW}Files where OCaml parser failed ($ocaml_fail_count):${RESET}"
    echo -e "$ocaml_fail_files" | grep -v "^$" | head -10 | while read file; do
        echo "  $file"
    done
    if [[ $ocaml_fail_count -gt 10 ]]; then
        remaining=$((ocaml_fail_count - 10))
        echo "  ... and $remaining more"
    fi
    echo ""
fi

echo -e "${BOLD}============================================${RESET}"

# Determine exit status
if [[ $diff_count -eq 0 && $pass_count -gt 0 ]]; then
    echo -e "${GREEN}${BOLD}SUCCESS: All $pass_count comparable files have identical ASTs!${RESET}"
    exit 0
elif [[ $pass_count -eq 0 ]]; then
    echo -e "${RED}${BOLD}ERROR: No files could be compared${RESET}"
    exit 1
else
    pct_match=$(python3 -c "print(f'{100 * $pass_count / ($pass_count + $diff_count):.1f}')")
    echo -e "${RED}${BOLD}FAILED: $diff_count files have AST differences ($pct_match% match)${RESET}"
    exit 1
fi
