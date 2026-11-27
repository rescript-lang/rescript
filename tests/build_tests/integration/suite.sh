#!/bin/bash

set -e

unset CLICOLOR_FORCE

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 <rescript executable>"
  exit 1
fi

RESCRIPT_EXECUTABLE="$(realpath "$1")"
export RESCRIPT_EXECUTABLE

# Make sure we are in the right directory
cd $(dirname $0)

if [[ "$RESCRIPT_EXECUTABLE" == */cli/rescript.js ]]; then
  echo "Using rescript CLI script: $RESCRIPT_EXECUTABLE"
else
  echo "Using rescript executable: $RESCRIPT_EXECUTABLE"
  eval $(node ./get_bin_paths.js)
  export RESCRIPT_RUNTIME
  echo Using runtime path: $RESCRIPT_RUNTIME
fi

source ./utils.sh

# Processes that run the executable under test, directly or, for the
# cli/rescript.js script, through node. Processes that only name it among
# their other arguments, like this script and the make that started it, don't
# count.
# The ps of Git for Windows has no -o and lists only the program, without its
# arguments, so there the plain match is enough. grep finding nothing is the
# normal case and must not end the script under set -e.
executable_processes() {
  if ps -eo pid=,args= >/dev/null 2>&1; then
    ps -eo pid=,args= | awk -v exe="$RESCRIPT_EXECUTABLE" \
      '$2 == exe || ($2 ~ /(^|\/)node$/ && $3 == exe)'
  else
    ps aux | grep -F "$RESCRIPT_EXECUTABLE" | grep -v grep || true
  fi
}

bold "Check for stale rescript processes"
STALE_PROCESSES=$(executable_processes)
if [ -n "$STALE_PROCESSES" ]; then
  error "Found stale rescript processes using this executable:"
  echo "$STALE_PROCESSES"
  exit 1
fi
success "No stale rescript processes found"

# The published ReScript versions model dependency layouts; compilation uses
# the repository-built compiler and runtime. See ../testrepo/README.md.
bold "Yarn install"
(cd ../testrepo && yarn)
node ./add-belt-dependencies.mjs

bold "Rescript version"
(cd ../testrepo && ./node_modules/.bin/rescript --version)

# we need to reset the yarn.lock and package.json to the original state
# so there is not diff in git. The CI will install new ReScript package
bold "Reset package.json and yarn.lock"
git restore --worktree -- ../testrepo/yarn.lock ../testrepo/package.json &> /dev/null
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
./compile/17-prod-flag.sh &&
./compile/18-external-dep-uncurried-dot.sh &&
./compile/19-utf8-warning.sh &&
./compile/20-schedule-independent-modules-after-error.sh &&
./compile/21-rebuild-dependency-for-package-specs.sh &&
./compile/22-qualified-own-namespace-dependency.sh &&
./compile/23-root-source-dir-rebuild.sh &&
./compile/14-no-testrepo-changes.sh &&
./compile/15-no-new-files.sh &&
./compile/16-snapshots-unchanged.sh &&

# GenType tests
./gentype/01-gentype-project-root.sh &&

# Watch tests
./watch/01-watch-recompile.sh &&
./watch/02-watch-warnings-persist.sh &&
./watch/02-watch-warnings-persist-atomic-save.sh &&
./watch/03-watch-new-file.sh &&
./watch/04-watch-config-change.sh &&
./watch/05-watch-ignores-non-source.sh &&
./watch/06-watch-missing-source-folder.sh &&
./watch/07-changed-interface-after-failed-implementation.sh &&
./watch/08-watch-overlapping-source-removed.sh &&
./watch/09-watch-recovers-from-invalid-config.sh &&
./watch/10-watch-recovers-new-dependency-config.sh &&

# Lock tests
./lock/01-lock-when-watching.sh &&

# Suffix tests
./suffix/01-custom-suffix.sh &&

# Format tests
./format/01-format-all-files.sh &&
./format/02-format-single-file.sh &&
./format/03-format-stdin.sh &&
./format/04-format-current-project.sh &&
./format/05-format-line-endings.sh &&

# Clean tests
./clean/01-clean-single-project.sh &&
./clean/02-clean-dev-dependencies.sh &&
./clean/03-clean-node-modules.sh &&
./clean/04-clean-rebuild-no-compiler-update.sh &&
./clean/05-clean-invalid-module-graph.sh &&
./clean/06-clean-duplicate-names-across-packages.sh &&

# Experimental tests
./experimental/01-experimental-features-emit.sh &&
./experimental/02-experimental-features-parse-error.sh &&
./experimental/03-watch-invalid-experimental.sh &&

# Experimental-invalid tests
./experimental-invalid/01-invalid-experimental-key.sh &&

# Features tests
./features/01-features-default-all-active.sh &&
./features/02-features-cli-restricts-to-one.sh &&
./features/03-features-transitive-expansion.sh &&
./features/04-features-toggle-cleans-artifacts.sh &&
./features/05-features-cycle-errors.sh &&
./features/06-features-empty-flag-rejected.sh &&

# Compiler-args tests
./compiler-args/01-compiler-args-cwd-invariant.sh &&
./compiler-args/02-warnings-in-parser-and-compiler.sh
