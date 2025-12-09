#!/bin/bash
# Test order-independence: verify that shuffling file order produces identical results
# This test runs the analysis multiple times with different file orderings.

set -e

warningYellow='\033[0;33m'
successGreen='\033[0;32m'
reset='\033[0m'

if [ "$RUNNER_OS" == "Windows" ]; then
  exclude_dirs="src\exception"
  suppress="src\ToSuppress.res"
else
  exclude_dirs="src/exception"
  suppress="src/ToSuppress.res"
fi

# Run analysis without shuffle (baseline)
baseline_output=$(mktemp)
dune exec rescript-editor-analysis -- reanalyze -dce -ci -exclude-paths $exclude_dirs -live-names globallyLive1 -live-names globallyLive2,globallyLive3 -suppress $suppress > "$baseline_output" 2>&1

# Run analysis with shuffle (3 times to increase confidence)
for i in 1 2 3; do
  shuffled_output=$(mktemp)
  dune exec rescript-editor-analysis -- reanalyze -dce -ci -test-shuffle -exclude-paths $exclude_dirs -live-names globallyLive1 -live-names globallyLive2,globallyLive3 -suppress $suppress > "$shuffled_output" 2>&1
  
  # Compare outputs
  if ! diff -q "$baseline_output" "$shuffled_output" > /dev/null 2>&1; then
    printf "${warningYellow}⚠️ Order-independence test failed on iteration $i!${reset}\n"
    printf "Baseline and shuffled outputs differ:\n"
    diff "$baseline_output" "$shuffled_output" || true
    rm -f "$baseline_output" "$shuffled_output"
    exit 1
  fi
  rm -f "$shuffled_output"
done

rm -f "$baseline_output"
printf "${successGreen}✅ Order-independence test passed (3 shuffled runs matched baseline).${reset}\n"

