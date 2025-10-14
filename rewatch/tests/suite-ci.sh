#!/bin/bash

unset CLICOLOR_FORCE

# Make sure we are in the right directory
cd $(dirname $0)

# Get rewatch executable location from the first argument or use default
if [ -n "$1" ]; then
  REWATCH_EXECUTABLE="$1"
else
  REWATCH_EXECUTABLE="../target/release/rescript"
  eval $(node ./get_bin_paths.js)
fi

export REWATCH_EXECUTABLE
export RESCRIPT_BSC_EXE
export RESCRIPT_RUNTIME

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

./compile.sh && ./watch.sh && ./lock.sh && ./suffix.sh && ./format.sh && ./clean.sh && ./experimental.sh && ./experimental-invalid.sh && ./compiler-args.sh && ./embeds-compiler.sh && ./embeds-nested-compiler.sh && ./embeds.sh && ./embeds-cache.sh && ./embeds-diags.sh && bash ./embeds-diags-compiler-log.sh && bash ./schema-embeds.sh && ./embeds-config.sh
