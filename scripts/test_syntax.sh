#!/bin/bash

# Note:
# 1. This was converted from zsh to bash because zsh is not available on Linux and Windows Github action runners.
# 2. macOS still has bash 3 and therefore no globstar ("**") support.
#    Therefore we need to use find + temp files for the file lists.

scriptDir=`dirname $0`
# macOS 12 does not have the realpath utility,
# so let's use this workaround instead.
PROJECT_ROOT=`cd "$scriptDir/.."; pwd -P`
DUNE_BIN_DIR="$PROJECT_ROOT/_build/install/default/bin"

# Allow switching between OCaml and Rust parsers via PARSER environment variable
# Usage: PARSER=rust ./scripts/test_syntax.sh
if [[ "$PARSER" == "rust" ]]; then
  RES_PARSER="$PROJECT_ROOT/compiler-rust/target/release/res_parser_rust"
  if [[ ! -x "$RES_PARSER" ]]; then
    echo "Error: Rust parser not found at $RES_PARSER"
    echo "Run 'cargo build --manifest-path compiler-rust/Cargo.toml --release' first."
    exit 1
  fi
  echo "Using Rust parser: $RES_PARSER"
else
  RES_PARSER="$DUNE_BIN_DIR/res_parser"
  if [[ ! -x "$RES_PARSER" ]]; then
    echo "Error: OCaml parser not found at $RES_PARSER"
    echo "Run 'make' first to build the OCaml parser."
    exit 1
  fi
fi

function exp {
  echo "$(dirname $1)/expected/$(basename $1).txt"
}

taskCount=0
function maybeWait {
  let taskCount+=1
  # spawn in batch of 20 processes
  [[ $((taskCount % 20)) = 0 ]] && wait
}

pushd tests

rm -rf temp
mkdir temp

# When using Rust parser, write to temp files and compare against expected files
# instead of overwriting them. This preserves the OCaml reference output.
if [[ "$PARSER" == "rust" ]]; then
  function out {
    # Output to temp directory, mirroring the expected file structure
    local expected=$(exp $1)
    local temp_out="temp/$expected"
    mkdir -p "$(dirname $temp_out)"
    echo "$temp_out"
  }
else
  function out {
    exp $1
  }
fi

# parsing
find syntax_tests/data/parsing/{errors,infiniteLoops,recovery} -name "*.res" -o -name "*.resi" >temp/files.txt
while read file; do
  $RES_PARSER -recover -print ml $file &> $(out $file) & maybeWait
done <temp/files.txt
find syntax_tests/data/parsing/{grammar,other} -name "*.res" -o -name "*.resi" >temp/files.txt
while read file; do
  $RES_PARSER -print ml $file &> $(out $file) & maybeWait
done <temp/files.txt

# printing
find syntax_tests/data/{printer,conversion} -name "*.res" -o -name "*.resi" -o -name "*.ml" -o -name "*.mli" >temp/files.txt
while read file; do
  $RES_PARSER $file &> $(out $file) & maybeWait
done <temp/files.txt

# printing with ast conversion
find syntax_tests/data/ast-mapping -name "*.res" -o -name "*.resi" -o -name "*.ml" -o -name "*.mli" >temp/files.txt
while read file; do
  $RES_PARSER -test-ast-conversion -jsx-version 4 $file &> $(out $file) & maybeWait
done <temp/files.txt

# printing with ppx
find syntax_tests/data/ppx/react -name "*.res" -o -name "*.resi" >temp/files.txt
while read file; do
  $RES_PARSER -jsx-version 4 $file &> $(out $file) & maybeWait
done <temp/files.txt

wait

warningYellow='\033[0;33m'
successGreen='\033[0;32m'
reset='\033[0m'

if [[ "$PARSER" == "rust" ]]; then
  # Compare temp output against expected files without modifying expected files
  # Use files instead of associative arrays for bash 3 compatibility
  mkdir -p temp/categories

  find temp/syntax_tests -name "*.txt" >temp/comparison_files.txt

  # Collect pass/fail results
  while read temp_file; do
    expected_file="${temp_file#temp/}"
    if [[ -f "$expected_file" ]]; then
      # Extract category from path: syntax_tests/data/CATEGORY/...
      category=$(echo "$expected_file" | sed 's|syntax_tests/data/||' | cut -d'/' -f1-2 | sed 's|/expected.*||')
      # Simplify nested categories
      case "$category" in
        parsing/*) category="parsing_$(echo $category | cut -d'/' -f2)" ;;
        ppx/*) category="ppx_$(echo $category | cut -d'/' -f2)" ;;
        */*) category=$(echo $category | cut -d'/' -f1) ;;
      esac

      if diff -q "$expected_file" "$temp_file" >/dev/null 2>&1; then
        echo "1" >> "temp/categories/${category}_passed.txt"
        echo "PASS:$expected_file" >> temp/results.txt
      else
        echo "1" >> "temp/categories/${category}_failed.txt"
        echo "FAIL:$expected_file" >> temp/results.txt
      fi
    fi
  done <temp/comparison_files.txt

  # Print summary
  echo ""
  echo "=== Rust Parser Syntax Test Results ==="
  echo ""
  printf "%-25s %6s %6s %6s\n" "Category" "Passed" "Failed" "Total"
  printf "%-25s %6s %6s %6s\n" "--------" "------" "------" "-----"

  total_passed=0
  total_failed=0

  # Get all categories from the files
  for passed_file in temp/categories/*_passed.txt temp/categories/*_failed.txt; do
    [[ -f "$passed_file" ]] || continue
    cat_name=$(basename "$passed_file" | sed 's/_passed.txt$//' | sed 's/_failed.txt$//')
    echo "$cat_name"
  done | sort -u > temp/all_categories.txt

  while read cat; do
    p=0
    f=0
    [[ -f "temp/categories/${cat}_passed.txt" ]] && p=$(wc -l < "temp/categories/${cat}_passed.txt" | tr -d ' ')
    [[ -f "temp/categories/${cat}_failed.txt" ]] && f=$(wc -l < "temp/categories/${cat}_failed.txt" | tr -d ' ')
    t=$((p + f))
    if [[ $t -gt 0 ]]; then
      pct=$((p * 100 / t))
      # Convert underscore back to slash for display
      display_cat=$(echo "$cat" | sed 's/_/\//')
      printf "%-25s %6d %6d %6d  (%2d%%)\n" "$display_cat" "$p" "$f" "$t" "$pct"
      total_passed=$((total_passed + p))
      total_failed=$((total_failed + f))
    fi
  done < temp/all_categories.txt

  total=$((total_passed + total_failed))
  printf "%-25s %6s %6s %6s\n" "--------" "------" "------" "-----"
  if [[ $total -gt 0 ]]; then
    pct=$((total_passed * 100 / total))
    printf "%-25s %6d %6d %6d  (%2d%%)\n" "TOTAL" "$total_passed" "$total_failed" "$total" "$pct"
  fi
  echo ""

  if [[ $total_failed -eq 0 ]]; then
    printf "${successGreen}✅ Rust parser output matches all expected files.${reset}\n"
    rm -r temp/
  else
    printf "${warningYellow}⚠️ $total_failed tests have differences${reset}\n"
    echo ""
    echo "Test artifacts preserved in tests/temp/"
    echo ""
    echo "To view a specific diff:"
    echo "  diff tests/syntax_tests/data/PATH/expected/FILE.txt tests/temp/syntax_tests/data/PATH/expected/FILE.txt"
    echo ""
    echo "To list all failing tests:"
    echo "  grep '^FAIL:' tests/temp/results.txt | cut -d: -f2"
    echo ""
    echo "To view first failing diff:"
    first_fail=$(grep '^FAIL:' temp/results.txt | head -1 | cut -d: -f2)
    if [[ -n "$first_fail" ]]; then
      echo "  diff $first_fail temp/$first_fail"
      echo ""
      echo "--- First failing test: $first_fail ---"
      diff --unified "$first_fail" "temp/$first_fail" | head -50
      diff_lines=$(diff --unified "$first_fail" "temp/$first_fail" | wc -l)
      if [[ $diff_lines -gt 50 ]]; then
        echo "... ($((diff_lines - 50)) more lines)"
      fi
    fi
    exit 1
  fi
else
  git diff --ignore-cr-at-eol $(find syntax_tests -name expected) >temp/diff.txt
  diff=$(cat temp/diff.txt)
  if [[ $diff = "" ]]; then
    printf "${successGreen}✅ No unstaged tests difference.${reset}\n"
  else
    printf "${warningYellow}⚠️ There are unstaged differences in syntax_tests/data/! Did you break a test?\n${diff}\n${reset}"
    rm -r temp/
    exit 1
  fi
fi

# roundtrip tests
if [[ $ROUNDTRIP_TEST = 1 ]]; then
  echo "Running roundtrip tests…"
  roundtripTestsResult="temp/result.txt"
  touch $roundtripTestsResult

  find syntax_tests/data/{idempotency,printer} -name "*.res" -o -name "*.resi" >temp/files.txt
  while read file; do {
    mkdir -p temp/$(dirname $file)
    sexpAst1=temp/$file.sexp
    sexpAst2=temp/$file.2.sexp
    sexpAst3=temp/$file.3.sexp
    rescript1=temp/$file.res
    rescript2=temp/$file.2.res

    case $file in
      *.res  ) resIntf=""         ;;
      *.resi ) resIntf=-interface ;;
    esac

    # First pass: original file -> AST1 and text1
    $RES_PARSER $resIntf -print sexp $file > $sexpAst1
    $RES_PARSER $resIntf -print res $file > $rescript1

    # Second pass: text1 -> AST2 and text2
    $RES_PARSER $resIntf -print sexp $rescript1 > $sexpAst2
    $RES_PARSER $resIntf -print res $rescript1 > $rescript2

    # Third pass: text2 -> AST3 (to check idempotency after normalization)
    $RES_PARSER $resIntf -print sexp $rescript2 > $sexpAst3

    # Check AST idempotency: AST2 should equal AST3 (allows AST1 != AST2 for canonicalization)
    diff --unified $sexpAst2 $sexpAst3
    [[ "$?" = 1 ]] && echo 1 > $roundtripTestsResult
    # Check text idempotency: text1 should equal text2
    diff --unified $rescript1 $rescript2
    [[ "$?" = 1 ]] && echo 1 > $roundtripTestsResult
  } & maybeWait
  done <temp/files.txt

  wait

  result=$(cat $roundtripTestsResult)

  if [[ $result = "1" ]]; then
    printf "${warningYellow}⚠️ Roundtrip tests failed.${reset}\n"
    exit 1
  else
    printf "${successGreen}✅ Roundtrip tests succeeded.${reset}\n"
  fi

fi

rm -r temp/
popd

printf "${successGreen}✅ All syntax tests passed.${reset}\n"
