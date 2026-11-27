#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: Deleting a file that other files depend on should fail compile"

rescript clean &> /dev/null
rescript build &> /dev/null

rm packages/dep02/src/Dep02.res
rescript build &> ../integration/snapshots/remove-file.txt
normalize_paths ../integration/snapshots/remove-file.txt
git restore --worktree -- packages/dep02/src/Dep02.res

rescript build &> /dev/null

# Check snapshot
if git diff --exit-code ../integration/snapshots/remove-file.txt &> /dev/null;
then
  success "Remove file snapshot is correct"
else
  error "Remove file snapshot changed"
  git diff ../integration/snapshots/remove-file.txt
  exit 1
fi
