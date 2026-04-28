#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: Rename file with interface should trigger error"

rewatch clean &> /dev/null
rewatch build &> /dev/null

mv ./packages/main/src/ModuleWithInterface.res ./packages/main/src/ModuleWithInterface2.res
rewatch build &> ../tests/snapshots/rename-file-with-interface.txt
normalize_paths ../tests/snapshots/rename-file-with-interface.txt
mv ./packages/main/src/ModuleWithInterface2.res ./packages/main/src/ModuleWithInterface.res

rewatch build &> /dev/null

# Check snapshot
if git diff --exit-code ../tests/snapshots/rename-file-with-interface.txt &> /dev/null;
then
  success "Rename file with interface snapshot is correct"
else
  error "Rename file with interface snapshot changed"
  git diff ../tests/snapshots/rename-file-with-interface.txt
  exit 1
fi
