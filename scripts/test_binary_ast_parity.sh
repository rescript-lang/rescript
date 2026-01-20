#!/bin/bash
# Test binary AST parity between Rust and OCaml compilers
#
# Usage: ./scripts/test_binary_ast_parity.sh [file.res ...]
#
# If no files specified, tests against syntax_tests/data/parsing/grammar/**/*.res

set -euo pipefail

RUST_BSC="./compiler-rust/target/debug/bsc"
OCAML_BSC="packages/@rescript/darwin-arm64/bin/bsc.exe"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

# Build Rust compiler
echo "Building Rust compiler..."
cargo build --manifest-path compiler-rust/Cargo.toml 2>/dev/null

# Statistics
total=0
matching=0
deps_match=0
size_diff=0
parse_error_rust=0
parse_error_ocaml=0

# Temporary files
RUST_AST=$(mktemp)
OCAML_AST=$(mktemp)
trap "rm -f $RUST_AST $OCAML_AST" EXIT

compare_file() {
    local file="$1"
    total=$((total + 1))

    # Generate Rust AST
    "$RUST_BSC" -bs-ast "$file" -o "$RUST_AST" 2>/dev/null || {
        parse_error_rust=$((parse_error_rust + 1))
        echo -e "${RED}R${NC} $file (Rust parse error)"
        return
    }

    # Generate OCaml AST
    "$OCAML_BSC" -bs-ast "$file" -o "$OCAML_AST" 2>/dev/null || {
        parse_error_ocaml=$((parse_error_ocaml + 1))
        echo -e "${RED}O${NC} $file (OCaml parse error)"
        return
    }

    # Check both files exist and have content
    if [ ! -s "$RUST_AST" ]; then
        parse_error_rust=$((parse_error_rust + 1))
        echo -e "${RED}R${NC} $file (Rust produced empty file)"
        return
    fi

    if [ ! -s "$OCAML_AST" ]; then
        parse_error_ocaml=$((parse_error_ocaml + 1))
        echo -e "${RED}O${NC} $file (OCaml produced empty file)"
        return
    fi

    # Compare full file
    if diff -q "$RUST_AST" "$OCAML_AST" > /dev/null 2>&1; then
        matching=$((matching + 1))
        echo -e "${GREEN}✓${NC} $file"
        return
    fi

    # Not fully matching - analyze differences

    # Extract dependency sections
    rust_dep_size=$(xxd -l 4 -p "$RUST_AST" | sed 's/../0x&,/g' | xargs printf "%d\n" 2>/dev/null || echo "0")
    ocaml_dep_size=$(xxd -l 4 -p "$OCAML_AST" | sed 's/../0x&,/g' | xargs printf "%d\n" 2>/dev/null || echo "0")

    # Extract just dependency bytes (bytes 4 to 4+dep_size)
    rust_deps=$(xxd -s 4 -l "$rust_dep_size" -p "$RUST_AST" 2>/dev/null || echo "")
    ocaml_deps=$(xxd -s 4 -l "$ocaml_dep_size" -p "$OCAML_AST" 2>/dev/null || echo "")

    if [ "$rust_deps" = "$ocaml_deps" ] && [ "$rust_dep_size" = "$ocaml_dep_size" ]; then
        deps_match=$((deps_match + 1))
    fi

    # Check size difference
    rust_size=$(wc -c < "$RUST_AST" | tr -d ' ')
    ocaml_size=$(wc -c < "$OCAML_AST" | tr -d ' ')

    if [ "$rust_size" != "$ocaml_size" ]; then
        size_diff=$((size_diff + 1))
        echo -e "${YELLOW}≠${NC} $file (Rust: ${rust_size}B, OCaml: ${ocaml_size}B)"
    else
        echo -e "${RED}✗${NC} $file (same size but different content)"
    fi
}

# Get list of files to test
if [ $# -gt 0 ]; then
    files=("$@")
else
    # Default: test grammar files
    mapfile -t files < <(find tests/syntax_tests/data/parsing/grammar -name "*.res" 2>/dev/null | head -100)
fi

echo "Testing ${#files[@]} files..."
echo ""

for file in "${files[@]}"; do
    compare_file "$file"
done

echo ""
echo "=== Summary ==="
echo "Total files: $total"
echo -e "${GREEN}Fully matching: $matching${NC}"
echo "Deps section match: $deps_match"
echo "Size different (likely sharing): $size_diff"
echo "Rust parse errors: $parse_error_rust"
echo "OCaml parse errors: $parse_error_ocaml"

if [ "$matching" -eq "$total" ]; then
    echo -e "${GREEN}All files match!${NC}"
    exit 0
else
    pct=$((matching * 100 / total))
    echo -e "${YELLOW}Match rate: ${pct}%${NC}"
    exit 1
fi
