#!/usr/bin/env bash
set -euo pipefail

# Run TypeScript typecheck and snapshot the expected errors
yarn typecheck > ts-errors.txt 2>&1 || true

# CI uses LF; normalize Windows CRLF just in case
if [ "${RUNNER_OS:-}" == "Windows" ]; then
  perl -pi -e 's/\r\n/\n/g' -- ts-errors.txt
fi

warningYellow='\033[0;33m'
successGreen='\033[0;32m'
reset='\033[0m'

diff=$(git ls-files --modified ts-errors.txt)
if [[ $diff = "" ]]; then
  printf "${successGreen}✅ TypeScript errors match snapshot (ts-errors.txt).${reset}\n"
else
  printf "${warningYellow}⚠️ Unexpected change in TypeScript error snapshot (ts-errors.txt).${reset}\n"
  git --no-pager diff -- ts-errors.txt
  exit 1
fi

