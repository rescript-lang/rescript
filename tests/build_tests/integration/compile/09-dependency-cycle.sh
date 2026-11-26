#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: It should show an error when we have a dependency cycle"

rewatch clean &> /dev/null
rewatch build &> /dev/null

echo 'Dep01.log()' >> packages/new-namespace/src/NS_alias.res
rewatch build &> ../integration/snapshots/dependency-cycle.txt
normalize_paths ../integration/snapshots/dependency-cycle.txt
git restore --worktree -- packages/new-namespace/src/NS_alias.res

rewatch build &> /dev/null

# Check snapshot
if git diff --exit-code ../integration/snapshots/dependency-cycle.txt &> /dev/null;
then
  success "Dependency cycle snapshot is correct"
else
  error "Dependency cycle snapshot changed"
  git diff ../integration/snapshots/dependency-cycle.txt
  exit 1
fi
