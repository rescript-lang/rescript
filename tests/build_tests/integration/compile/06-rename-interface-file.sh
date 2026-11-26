#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: Rename interface file should trigger error"

rewatch clean &> /dev/null
rewatch build &> /dev/null

mv ./packages/main/src/ModuleWithInterface.resi ./packages/main/src/ModuleWithInterface2.resi
rewatch build &> ../integration/snapshots/rename-interface-file.txt
normalize_paths ../integration/snapshots/rename-interface-file.txt
mv ./packages/main/src/ModuleWithInterface2.resi ./packages/main/src/ModuleWithInterface.resi

rewatch build &> /dev/null

# Check snapshot
if git diff --exit-code ../integration/snapshots/rename-interface-file.txt &> /dev/null;
then
  success "Rename interface file snapshot is correct"
else
  error "Rename interface file snapshot changed"
  git diff ../integration/snapshots/rename-interface-file.txt
  exit 1
fi
