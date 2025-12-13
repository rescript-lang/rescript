# Optional: pass PARALLEL=n to run in parallel mode (e.g., PARALLEL=4 ./test.sh)
# In parallel mode, we skip -debug flag since debug output is order-dependent
PARALLEL_FLAG=""
DEBUG_FLAG="-debug"
if [ -n "$PARALLEL" ]; then
  PARALLEL_FLAG="-parallel $PARALLEL"
  DEBUG_FLAG=""
fi

if [ "$RUNNER_OS" == "Windows" ]; then
  exclude_dirs="src\exception"
  suppress="src\ToSuppress.res"
else
  exclude_dirs="src/exception"
  suppress="src/ToSuppress.res"
fi

# For parallel mode, compare only the analysis summary line (issue counts)
if [ -n "$PARALLEL" ]; then
  # Run parallel analysis
  dune exec rescript-editor-analysis -- reanalyze -config -ci -exclude-paths $exclude_dirs -live-names globallyLive1 -live-names globallyLive2,globallyLive3 -suppress $suppress $PARALLEL_FLAG 2>/dev/null > /tmp/parallel-deadcode.txt
  
  # Extract the summary line (Analysis reported N issues...)
  expected_summary=$(grep "Analysis reported" expected/deadcode.txt)
  parallel_summary=$(grep "Analysis reported" /tmp/parallel-deadcode.txt)
  
  if [ "$expected_summary" = "$parallel_summary" ]; then
    printf "\033[0;32m✅ Parallel DCE produces identical issue counts: $parallel_summary\033[0m\n"
  else
    printf "\033[0;33m⚠️ Parallel DCE produced different results!\033[0m\n"
    echo "Expected: $expected_summary"
    echo "Got:      $parallel_summary"
    exit 1
  fi
  
  # Also run exception analysis in parallel
  if [ "$RUNNER_OS" == "Windows" ]; then
    unsuppress_dirs="src\exception"
  else
    unsuppress_dirs="src/exception"
  fi
  dune exec rescript-editor-analysis -- reanalyze -exception -ci -suppress src -unsuppress $unsuppress_dirs $PARALLEL_FLAG 2>/dev/null > /tmp/parallel-exception.txt
  
  expected_summary=$(grep "Analysis reported" expected/exception.txt)
  parallel_summary=$(grep "Analysis reported" /tmp/parallel-exception.txt)
  
  if [ "$expected_summary" = "$parallel_summary" ]; then
    printf "\033[0;32m✅ Parallel exception analysis produces identical issue counts: $parallel_summary\033[0m\n"
  else
    printf "\033[0;33m⚠️ Parallel exception analysis produced different results!\033[0m\n"
    echo "Expected: $expected_summary"
    echo "Got:      $parallel_summary"
    exit 1
  fi
  
  exit 0
fi

# Sequential mode - generate expected files
output="expected/deadcode.txt"
dune exec rescript-editor-analysis -- reanalyze -config $DEBUG_FLAG -ci -exclude-paths $exclude_dirs -live-names globallyLive1 -live-names globallyLive2,globallyLive3 -suppress $suppress > $output
# CI. We use LF, and the CI OCaml fork prints CRLF. Convert.
if [ "$RUNNER_OS" == "Windows" ]; then
  perl -pi -e 's/\r\n/\n/g' -- $output
fi

output="expected/exception.txt"
if [ "$RUNNER_OS" == "Windows" ]; then
  unsuppress_dirs="src\exception"
else
  unsuppress_dirs="src/exception"
fi
dune exec rescript-editor-analysis -- reanalyze -exception -ci -suppress src -unsuppress $unsuppress_dirs > $output
# CI. We use LF, and the CI OCaml fork prints CRLF. Convert.
if [ "$RUNNER_OS" == "Windows" ]; then
  perl -pi -e 's/\r\n/\n/g' -- $output
fi


warningYellow='\033[0;33m'
successGreen='\033[0;32m'
reset='\033[0m'

diff=$(git ls-files --modified expected)
if [[ $diff = "" ]]; then
  printf "${successGreen}✅ No unstaged tests difference.${reset}\n"
else
  printf "${warningYellow}⚠️ There are unstaged differences in tests/! Did you break a test?\n${diff}\n${reset}"
  git --no-pager diff expected
  exit 1
fi
