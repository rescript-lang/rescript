#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: Rename a file with a dependent in a namespaced package should trigger an error (regression)"

rewatch clean &> /dev/null
rewatch build &> /dev/null

mv ./packages/new-namespace/src/Other_module.res ./packages/new-namespace/src/Other_module2.res
rewatch build &> ../tests/snapshots/rename-file-internal-dep-namespace.txt
normalize_paths ../tests/snapshots/rename-file-internal-dep-namespace.txt
mv ./packages/new-namespace/src/Other_module2.res ./packages/new-namespace/src/Other_module.res

rewatch build &> /dev/null

# Check snapshot
if git diff --exit-code ../tests/snapshots/rename-file-internal-dep-namespace.txt &> /dev/null;
then
  success "Rename file namespace snapshot is correct"
else
  error "Rename file namespace snapshot changed"
  git diff ../tests/snapshots/rename-file-internal-dep-namespace.txt
  exit 1
fi
