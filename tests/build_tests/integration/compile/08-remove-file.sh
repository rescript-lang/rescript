#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: Deleting a file that other files depend on should fail compile"

rewatch clean &> /dev/null
rewatch build &> /dev/null

rm packages/dep02/src/Dep02.res
rewatch build &> ../integration/snapshots/remove-file.txt
normalize_paths ../integration/snapshots/remove-file.txt
git restore --worktree -- packages/dep02/src/Dep02.res

rewatch build &> /dev/null

# Check snapshot
if git diff --exit-code ../integration/snapshots/remove-file.txt &> /dev/null;
then
  success "Remove file snapshot is correct"
else
  error "Remove file snapshot changed"
  git diff ../integration/snapshots/remove-file.txt
  exit 1
fi
