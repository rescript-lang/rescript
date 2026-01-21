#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: Rename file should trigger error"

rewatch clean &> /dev/null
rewatch build &> /dev/null

node ./packages/main/src/Main.mjs > ./packages/main/src/output.txt

mv ./packages/main/src/Main.res ./packages/main/src/Main2.res
rewatch build &> ../tests/snapshots/rename-file.txt
normalize_paths ../tests/snapshots/rename-file.txt
mv ./packages/main/src/Main2.res ./packages/main/src/Main.res

rewatch build &> /dev/null

# Check snapshot
if git diff --exit-code ../tests/snapshots/rename-file.txt &> /dev/null;
then
  success "Rename file snapshot is correct"
else
  error "Rename file snapshot changed"
  git diff ../tests/snapshots/rename-file.txt
  exit 1
fi
