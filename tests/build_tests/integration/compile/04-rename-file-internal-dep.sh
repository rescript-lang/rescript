#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: Rename a file with a dependent should trigger an error"

rescript clean &> /dev/null
rescript build &> /dev/null

mv ./packages/main/src/InternalDep.res ./packages/main/src/InternalDep2.res
rescript build &> ../integration/snapshots/rename-file-internal-dep.txt
normalize_paths ../integration/snapshots/rename-file-internal-dep.txt
mv ./packages/main/src/InternalDep2.res ./packages/main/src/InternalDep.res

rescript build &> /dev/null

# Check snapshot
if git diff --exit-code ../integration/snapshots/rename-file-internal-dep.txt &> /dev/null;
then
  success "Rename file internal dep snapshot is correct"
else
  error "Rename file internal dep snapshot changed"
  git diff ../integration/snapshots/rename-file-internal-dep.txt
  exit 1
fi
