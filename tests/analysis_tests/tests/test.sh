for file in src/*.{res,resi}; do
  output="$(dirname $file)/expected/$(basename $file).txt"
  ../../../_build/install/default/bin/rescript-editor-analysis test $file &> $output
  # CI. We use LF, and the CI OCaml fork prints CRLF. Convert.
  if [ "$RUNNER_OS" == "Windows" ]; then
    perl -pi -e 's/\r\n/\n/g' -- $output
  fi
done

for file in not_compiled/*.{res,resi}; do
  output="$(dirname $file)/expected/$(basename $file).txt"
  ../../../_build/install/default/bin/rescript-editor-analysis test $file &> $output
  # CI. We use LF, and the CI OCaml fork prints CRLF. Convert.
  if [ "$RUNNER_OS" == "Windows" ]; then
    perl -pi -e 's/\r\n/\n/g' -- $output
  fi
done

warningYellow='\033[0;33m'
successGreen='\033[0;32m'
reset='\033[0m'

diff=$(git ls-files --modified src/expected)
if [[ $diff = "" ]]; then
  printf "${successGreen}✅ No analysis_tests snapshot changes detected.${reset}\n"
else
  printf "${warningYellow}⚠️ The analysis_tests snapshot doesn't match. Double check that the output is correct, run 'make analysis_tests' and stage the diff.\n${diff}\n${reset}"
  git --no-pager diff src/expected
  exit 1
fi
