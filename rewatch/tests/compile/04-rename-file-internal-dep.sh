#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: Rename a file with a dependent should trigger an error"

rewatch clean &> /dev/null
rewatch build &> /dev/null

mv ./packages/main/src/InternalDep.res ./packages/main/src/InternalDep2.res
rewatch build &> ../tests/snapshots/rename-file-internal-dep.txt
normalize_paths ../tests/snapshots/rename-file-internal-dep.txt
mv ./packages/main/src/InternalDep2.res ./packages/main/src/InternalDep.res

rewatch build &> /dev/null

# Check snapshot
if git diff --exit-code ../tests/snapshots/rename-file-internal-dep.txt &> /dev/null;
then
  success "Rename file internal dep snapshot is correct"
else
  error "Rename file internal dep snapshot changed"
  git diff ../tests/snapshots/rename-file-internal-dep.txt
  exit 1
fi
