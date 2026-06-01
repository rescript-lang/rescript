for file in src/*.res; do
  output="$(dirname $file)/expected/$(basename $file).txt"
  ../../../_build/install/default/bin/rescript-editor-analysis test $file &> $output
  # CI. We use LF, and the CI OCaml fork prints CRLF. Convert.
  if [ "$RUNNER_OS" == "Windows" ]; then
    perl -pi -e 's/\r\n/\n/g' -- $output
  fi

  # Remove unwanted output (machine-specific path)
  perl -ni -e 'print unless /^\[getRuntimeDir\]/' -- $output

  # Strip leading newlines caused by ^in+ and ^dv+ marker usage.
  perl -0pi -e 's/\A\n+//' -- $output

  # Strip trailing newlines caused by ^dv- and ^in- marker usage.
  perl -0pi -e 's/\n+\z/\n/' -- $output
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
