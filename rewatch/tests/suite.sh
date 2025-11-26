#!/bin/bash

set -e

unset CLICOLOR_FORCE

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 <rewatch executable>"
  exit 1
fi

REWATCH_EXECUTABLE="$(realpath "$1")"
export REWATCH_EXECUTABLE

# Make sure we are in the right directory
cd $(dirname $0)

if [[ "$REWATCH_EXECUTABLE" == */cli/rescript.js ]]; then
  echo "Using rewatch CLI script: $REWATCH_EXECUTABLE"
else
  echo "Using rewatch executable: $REWATCH_EXECUTABLE"
  eval $(node ./get_bin_paths.js)
  export RESCRIPT_BSC_EXE
  export RESCRIPT_RUNTIME
  echo Using bsc executable: $RESCRIPT_BSC_EXE
  echo Using runtime path: $RESCRIPT_RUNTIME
fi

source ./utils.sh

bold "Yarn install"
(cd ../testrepo && yarn)

bold "Rescript version"
(cd ../testrepo && ./node_modules/.bin/rescript --version)

# we need to reset the yarn.lock and package.json to the original state
# so there is not diff in git. The CI will install new ReScript package
bold "Reset package.json and yarn.lock"
git checkout ../testrepo/yarn.lock &> /dev/null
git checkout ../testrepo/package.json &> /dev/null
success "Reset package.json and yarn.lock"

bold "Make sure the testrepo is clean"
if git diff --exit-code ../testrepo &> diff.txt;
then
  rm diff.txt
  success "Testrepo has no changes"
else
  error "Testrepo is not clean to start with"
  cat diff.txt
  rm diff.txt
  exit 1
fi

./compile.sh && ./watch.sh && ./lock.sh && ./suffix.sh && ./format.sh && ./clean.sh && ./experimental.sh && ./experimental-invalid.sh && ./compiler-args.sh && ./mcp.sh
