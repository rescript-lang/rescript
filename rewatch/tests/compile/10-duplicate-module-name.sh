#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: It should show an error for duplicate module names"

rewatch clean &> /dev/null
rewatch build &> /dev/null

mkdir -p packages/main/src/dupe-a packages/main/src/dupe-b
echo 'let value = 1' > packages/main/src/dupe-a/DuplicateModule.res
echo 'let value = 2' > packages/main/src/dupe-b/DuplicateModule.res
rewatch build &> ../tests/snapshots/duplicate-module-name.txt
normalize_paths ../tests/snapshots/duplicate-module-name.txt
rm -rf packages/main/src/dupe-a packages/main/src/dupe-b

rewatch build &> /dev/null

# Check snapshot
if git diff --exit-code ../tests/snapshots/duplicate-module-name.txt &> /dev/null;
then
  success "Duplicate module name snapshot is correct"
else
  error "Duplicate module name snapshot changed"
  git diff ../tests/snapshots/duplicate-module-name.txt
  exit 1
fi
