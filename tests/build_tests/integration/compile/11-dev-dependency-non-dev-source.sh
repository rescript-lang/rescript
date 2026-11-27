#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: Dev dependency used by non-dev source should not compile"

rescript clean &> /dev/null
rescript build &> /dev/null

# This should not compile because "@rescript/webapi" is part of dev-dependencies
# and FileToTest.res is not listed as "type":"dev"
echo 'open WebAPI' >> packages/with-dev-deps/src/FileToTest.res
rescript build &> ../integration/snapshots/dev-dependency-used-by-non-dev-source.txt
normalize_paths ../integration/snapshots/dev-dependency-used-by-non-dev-source.txt
git restore --worktree -- packages/with-dev-deps/src/FileToTest.res

rescript build &> /dev/null

# Check snapshot
if git diff --exit-code ../integration/snapshots/dev-dependency-used-by-non-dev-source.txt &> /dev/null;
then
  success "Dev dependency non-dev source snapshot is correct"
else
  error "Dev dependency non-dev source snapshot changed"
  git diff ../integration/snapshots/dev-dependency-used-by-non-dev-source.txt
  exit 1
fi
