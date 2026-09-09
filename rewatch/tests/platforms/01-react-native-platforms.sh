#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")"
source "../utils.sh"

: "${REWATCH_EXECUTABLE:?Set REWATCH_EXECUTABLE to the Rewatch binary under test}"
if [ -z "${RESCRIPT_BSC_EXE:-}" ] || [ -z "${RESCRIPT_RUNTIME:-}" ]; then
  eval "$(node ../get_bin_paths.js)"
  export RESCRIPT_BSC_EXE RESCRIPT_RUNTIME
fi

bold "Test: React Native platform modules through Rewatch"
node --test acceptance.test.mjs
