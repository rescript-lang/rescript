for file in src/App.res src/Button.android.res src/Button.ios.res; do
  output="$(dirname $file)/expected/$(basename $file).txt"
  ../../../_build/install/default/bin/rescript-editor-analysis test $file &> $output
  if [ "$RUNNER_OS" == "Windows" ]; then
    perl -pi -e 's/\r\n/\n/g' -- $output
  fi
done

warningYellow='\033[0;33m'
successGreen='\033[0;32m'
reset='\033[0m'

diff=$(git ls-files --modified src/expected)
if [[ $diff = "" ]]; then
  printf "${successGreen}✅ No platform module analysis snapshot changes detected.${reset}\n"
else
  printf "${warningYellow}⚠️ The platform module analysis snapshot changed.\n${diff}\n${reset}"
  git --no-pager diff src/expected
  exit 1
fi
