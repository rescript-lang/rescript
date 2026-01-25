#!/bin/bash
# Generate benchmark source files by replicating the deadcode test files
# Usage: ./generate.sh [num_copies]

set -e

NUM_COPIES=${1:-10}
SRC_DIR="../deadcode/src"
DEST_DIR="src"

echo "Generating benchmark with $NUM_COPIES copies..."

rm -rf "$DEST_DIR"
mkdir -p "$DEST_DIR" "$DEST_DIR/exception"

# Collect module names into a file
MODULES_FILE="/tmp/modules_$$"
find "$SRC_DIR" \( -name "*.res" -o -name "*.resi" \) | while read f; do
  filename=$(basename "$f")
  echo "${filename%.*}"
done | sort -u > "$MODULES_FILE"

NUM_MODULES=$(wc -l < "$MODULES_FILE")
echo "Found $NUM_MODULES unique modules"

# Generate perl script template
PERL_TEMPLATE="/tmp/gen_$$.pl"

for n in $(seq 1 $NUM_COPIES); do
  echo -n "Copy $n: "
  
  # Build perl script for this copy number
  {
    echo 'while (<STDIN>) {'
    while read mod; do
      echo "  s/(?<![A-Za-z0-9_])${mod}(?![A-Za-z0-9_])/${mod}_${n}/g;"
    done < "$MODULES_FILE"
    echo '  print;'
    echo '}'
  } > "$PERL_TEMPLATE"
  
  # Process main source files
  for f in $(find "$SRC_DIR" -maxdepth 1 \( -name "*.res" -o -name "*.resi" \)); do
    filename=$(basename "$f")
    ext="${filename##*.}"
    base="${filename%.*}"
    perl "$PERL_TEMPLATE" < "$f" > "$DEST_DIR/${base}_${n}.${ext}"
  done
  
  # Process exception files
  for f in $(find "$SRC_DIR/exception" \( -name "*.res" -o -name "*.resi" \) 2>/dev/null); do
    filename=$(basename "$f")
    ext="${filename##*.}"
    base="${filename%.*}"
    perl "$PERL_TEMPLATE" < "$f" > "$DEST_DIR/exception/${base}_${n}.${ext}"
  done
  
  echo "done"
done

rm -f "$MODULES_FILE" "$PERL_TEMPLATE"
total=$(find "$DEST_DIR" \( -name "*.res" -o -name "*.resi" \) | wc -l)
echo "Generated $total files"

