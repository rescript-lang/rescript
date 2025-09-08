for file in src/*.{res,resi}; do
  output="$(dirname $file)/expected/$(basename $file).json"
  ../../_build/install/default/bin/rescript-tools doc $file > $output
  # # CI. We use LF, and the CI OCaml fork prints CRLF. Convert.
  if [ "$RUNNER_OS" == "Windows" ]; then
    perl -pi -e 's/\r\n/\n/g' -- $output
  fi
done

for file in ppx/*.res; do
  output="src/expected/$(basename $file).jsout"
  ../../cli/bsc.js -ppx "../../_build/install/default/bin/rescript-tools ppx" $file > $output
  if [ "$RUNNER_OS" == "Windows" ]; then
    perl -pi -e 's/\r\n/\n/g' -- $output
  fi
done

# Test format-codeblocks command
for file in src/docstrings-format/*.{res,resi,md}; do
  output="src/expected/$(basename $file).expected"
  ../../_build/install/default/bin/rescript-tools format-codeblocks "$file" --stdout > $output
  if [ "$RUNNER_OS" == "Windows" ]; then
    perl -pi -e 's/\r\n/\n/g' -- $output
  fi
done

# Test extract-codeblocks command
for file in src/docstrings-format/*.{res,resi,md}; do
  output="src/expected/$(basename $file).extracted.json.expected"
  ../../_build/install/default/bin/rescript-tools extract-codeblocks "$file" --transform-assert-equal > $output
  if [ "$RUNNER_OS" == "Windows" ]; then
    perl -pi -e 's/\r\n/\n/g' -- $output
  fi
done

warningYellow='\033[0;33m'
successGreen='\033[0;32m'
reset='\033[0m'

diff=$(git ls-files --modified src/expected)
if [[ $diff = "" ]]; then
  printf "${successGreen}✅ No unstaged tests difference.${reset}\n"
else
  printf "${warningYellow}⚠️ There are unstaged differences in tests/! Did you break a test?\n${diff}\n${reset}"
  git --no-pager diff src/expected
  exit 1
fi
