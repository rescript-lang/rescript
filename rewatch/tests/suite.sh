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
(cd ../testrepo && yarn && cp node_modules/rescript-nodejs/bsconfig.json node_modules/rescript-nodejs/rescript.json)

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

# Individual test files
# Comment out any test to skip it

# Compile tests
./compile/01-basic-compile.sh &&
./compile/02-standalone-package.sh &&
./compile/03-rename-file.sh &&
./compile/04-rename-file-internal-dep.sh &&
./compile/05-rename-file-namespace.sh &&
./compile/06-rename-interface-file.sh &&
./compile/07-rename-file-with-interface.sh &&
./compile/08-remove-file.sh &&
./compile/09-dependency-cycle.sh &&
./compile/10-duplicate-module-name.sh &&
./compile/11-dev-dependency-non-dev-source.sh &&
./compile/12-compile-dev-dependencies.sh &&
./compile/13-no-infinite-loop-with-cycle.sh &&
./compile/14-no-testrepo-changes.sh &&
./compile/15-no-new-files.sh &&
./compile/16-snapshots-unchanged.sh &&

# Watch tests
./watch/01-watch-recompile.sh &&
./watch/02-watch-warnings-persist.sh &&

# Lock tests
./lock/01-lock-when-watching.sh &&

# Suffix tests
./suffix/01-custom-suffix.sh &&

# Format tests
./format/01-format-all-files.sh &&
./format/02-format-single-file.sh &&
./format/03-format-stdin.sh &&
./format/04-format-current-project.sh &&

# Clean tests
./clean/01-clean-single-project.sh &&
./clean/02-clean-dev-dependencies.sh &&
./clean/03-clean-node-modules.sh &&
./clean/04-clean-rebuild-no-compiler-update.sh &&

# Experimental tests
./experimental/01-experimental-features-emit.sh &&
./experimental/02-experimental-features-parse-error.sh &&
./experimental/03-watch-invalid-experimental.sh &&

# Experimental-invalid tests
./experimental-invalid/01-invalid-experimental-key.sh &&

# Compiler-args tests
./compiler-args/01-compiler-args-cwd-invariant.sh &&
./compiler-args/02-warnings-in-parser-and-compiler.sh
