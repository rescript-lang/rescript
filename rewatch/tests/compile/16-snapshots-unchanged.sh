#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: See if the snapshots have changed"

changed_snapshots=$(git ls-files --modified ../tests/snapshots)
if git diff --exit-code ../tests/snapshots &> /dev/null;
then
  success "Snapshots are correct"
else
  error "Snapshots are incorrect:"
  printf "\n\n"
  for file in $changed_snapshots; do
    bold $file
    git diff $file $file
    printf "\n\n"
  done
  exit 1
fi
