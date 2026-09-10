#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: Clean succeeds when the module graph is invalid"

rewatch clean > /dev/null 2>&1
rewatch build > /dev/null 2>&1

mkdir -p packages/main/src/dupe-a packages/main/src/dupe-b
echo 'let value = 1' > packages/main/src/dupe-a/DuplicateModule.res
echo 'let value = 2' > packages/main/src/dupe-b/DuplicateModule.res

error_output=$(rewatch clean 2>&1)
clean_status=$?

rm -rf packages/main/src/dupe-a packages/main/src/dupe-b

if [ $clean_status -ne 0 ]; then
  error "Clean failed for an invalid module graph"
  printf "%s\n" "$error_output" >&2
  rewatch build > /dev/null 2>&1
  exit 1
fi

if [ -f packages/main/src/Main.bs.js ]; then
  error "Clean left an in-source output behind"
  rewatch build > /dev/null 2>&1
  exit 1
fi

rewatch build > /dev/null 2>&1
success "Invalid module graph cleaned"
