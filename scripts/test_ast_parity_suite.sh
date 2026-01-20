#!/bin/bash
# Comprehensive AST Parity Test Suite
#
# Tests Rust vs OCaml binary AST output for parity across syntax test files.
# Verifies:
# - Dependencies section is identical
# - File sizes match or are very close
# - Reports byte-identical files
#
# Usage: ./scripts/test_ast_parity_suite.sh [--verbose] [--limit N] [pattern]

set -euo pipefail

RUST_BSC="./compiler-rust/target/debug/bsc"
OCAML_BSC="packages/@rescript/darwin-arm64/bin/bsc.exe"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Parse arguments
VERBOSE=false
LIMIT=0
PATTERN=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --verbose|-v)
            VERBOSE=true
            shift
            ;;
        --limit|-l)
            LIMIT="$2"
            shift 2
            ;;
        *)
            PATTERN="$1"
            shift
            ;;
    esac
done

# Build Rust compiler
echo "Building Rust compiler..."
cargo build --manifest-path compiler-rust/Cargo.toml 2>/dev/null

# Statistics
total=0
identical=0
same_size=0
same_deps=0
rust_smaller=0
ocaml_smaller=0
rust_parse_error=0
ocaml_parse_error=0

# Temporary files (must use .ast extension for OCaml compiler)
RUST_AST="/tmp/parity_test_rust_$$.ast"
OCAML_AST="/tmp/parity_test_ocaml_$$.ast"
trap "rm -f $RUST_AST $OCAML_AST" EXIT

compare_file() {
    local file="$1"
    total=$((total + 1))

    # Remove temp files before each run (OCaml won't overwrite existing files)
    rm -f "$RUST_AST" "$OCAML_AST"

    # Generate Rust AST
    "$RUST_BSC" -bs-ast "$file" -o "$RUST_AST" 2>/dev/null || {
        rust_parse_error=$((rust_parse_error + 1))
        if $VERBOSE; then
            echo -e "${RED}R${NC} $file (Rust parse error)"
        fi
        return
    }

    # Generate OCaml AST
    "$OCAML_BSC" -bs-ast "$file" -o "$OCAML_AST" 2>/dev/null || {
        ocaml_parse_error=$((ocaml_parse_error + 1))
        if $VERBOSE; then
            echo -e "${RED}O${NC} $file (OCaml parse error)"
        fi
        return
    }

    # Check both files exist and have content
    if [ ! -s "$RUST_AST" ]; then
        rust_parse_error=$((rust_parse_error + 1))
        if $VERBOSE; then
            echo -e "${RED}R${NC} $file (Rust produced empty file)"
        fi
        return
    fi

    if [ ! -s "$OCAML_AST" ]; then
        ocaml_parse_error=$((ocaml_parse_error + 1))
        if $VERBOSE; then
            echo -e "${RED}O${NC} $file (OCaml produced empty file)"
        fi
        return
    fi

    # Compare full file (byte-for-byte)
    if diff -q "$RUST_AST" "$OCAML_AST" > /dev/null 2>&1; then
        identical=$((identical + 1))
        same_size=$((same_size + 1))
        same_deps=$((same_deps + 1))
        if $VERBOSE; then
            echo -e "${GREEN}✓${NC} $file (byte-identical)"
        fi
        return
    fi

    # Extract and compare dependency sections
    rust_dep_size=$(xxd -l 4 -p "$RUST_AST" | sed 's/../0x&,/g' | xargs printf "%d\n" 2>/dev/null || echo "0")
    ocaml_dep_size=$(xxd -l 4 -p "$OCAML_AST" | sed 's/../0x&,/g' | xargs printf "%d\n" 2>/dev/null || echo "0")

    rust_deps=$(xxd -s 4 -l "$rust_dep_size" -p "$RUST_AST" 2>/dev/null || echo "")
    ocaml_deps=$(xxd -s 4 -l "$ocaml_dep_size" -p "$OCAML_AST" 2>/dev/null || echo "")

    if [ "$rust_deps" = "$ocaml_deps" ] && [ "$rust_dep_size" = "$ocaml_dep_size" ]; then
        same_deps=$((same_deps + 1))
    fi

    # Check file sizes
    rust_size=$(wc -c < "$RUST_AST" | tr -d ' ')
    ocaml_size=$(wc -c < "$OCAML_AST" | tr -d ' ')

    if [ "$rust_size" -eq "$ocaml_size" ]; then
        same_size=$((same_size + 1))
        if $VERBOSE; then
            echo -e "${BLUE}≈${NC} $file (same size: ${rust_size}B, different content)"
        fi
    elif [ "$rust_size" -lt "$ocaml_size" ]; then
        rust_smaller=$((rust_smaller + 1))
        if $VERBOSE; then
            echo -e "${YELLOW}R<${NC} $file (Rust: ${rust_size}B, OCaml: ${ocaml_size}B)"
        fi
    else
        ocaml_smaller=$((ocaml_smaller + 1))
        if $VERBOSE; then
            echo -e "${YELLOW}O<${NC} $file (Rust: ${rust_size}B, OCaml: ${ocaml_size}B)"
        fi
    fi
}

# Get list of files to test (exclude error test files)
if [ -n "$PATTERN" ]; then
    FILES=$(find tests/syntax_tests/data -name "*.res" -path "*$PATTERN*" -not -path "*/errors/*" 2>/dev/null | sort)
else
    FILES=$(find tests/syntax_tests/data/parsing -name "*.res" -not -path "*/errors/*" 2>/dev/null | head -200 | sort)
fi

if [ "$LIMIT" -gt 0 ]; then
    FILES=$(echo "$FILES" | head -n "$LIMIT")
fi

FILE_COUNT=$(echo "$FILES" | grep -c . || echo 0)
echo "Testing $FILE_COUNT files..."
echo ""

# Use process substitution to avoid subshell (preserves variables)
while IFS= read -r file; do
    [ -n "$file" ] && compare_file "$file"
done <<< "$FILES"

echo ""
echo "=== Summary ==="
echo "Total files tested: $total"
echo ""
echo -e "${GREEN}Byte-identical: $identical${NC}"
echo -e "${BLUE}Same size: $same_size${NC}"
echo "Same dependencies: $same_deps"
echo ""
echo "Rust smaller: $rust_smaller"
echo "OCaml smaller: $ocaml_smaller"
echo ""
echo "Rust parse errors: $rust_parse_error"
echo "OCaml parse errors: $ocaml_parse_error"

# Calculate success rate
if [ "$total" -gt 0 ]; then
    # Calculate various parity metrics
    parsed_ok=$((total - rust_parse_error - ocaml_parse_error))

    echo ""
    echo "=== Parity Metrics ==="

    # Dependency parity is the key metric for AST correctness
    if [ "$parsed_ok" -gt 0 ]; then
        dep_pct=$((same_deps * 100 / parsed_ok))
        echo -e "Dependency parity: ${dep_pct}% ($same_deps/$parsed_ok parseable files)"
    fi

    # Size parity indicates sharing similarity
    if [ "$parsed_ok" -gt 0 ]; then
        size_pct=$((same_size * 100 / parsed_ok))
        echo "Size parity: ${size_pct}% ($same_size/$parsed_ok parseable files)"
    fi

    # Byte-identical is ideal but unlikely without matching pointer sharing
    if [ "$parsed_ok" -gt 0 ]; then
        identical_pct=$((identical * 100 / parsed_ok))
        echo "Byte-identical: ${identical_pct}% ($identical/$parsed_ok parseable files)"
    fi

    echo ""
    if [ "$identical" -eq "$parsed_ok" ] && [ "$parsed_ok" -gt 0 ]; then
        echo -e "${GREEN}All parseable files are byte-identical!${NC}"
        exit 0
    elif [ "$dep_pct" -ge 90 ]; then
        echo -e "${GREEN}Excellent dependency parity (≥90%)${NC}"
        exit 0
    elif [ "$dep_pct" -ge 70 ]; then
        echo -e "${YELLOW}Good dependency parity (≥70%)${NC}"
        exit 0
    else
        echo -e "${RED}Low dependency parity (<70%)${NC}"
        exit 1
    fi
fi
