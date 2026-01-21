#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

# If we clean a package, we should not see a "Cleaned previous build due to compiler update" message.
# Clean the whole repo and rebuild, then ensure the compiler-update clean message is absent
bold "Test: Clean repo then rebuild should not log compiler update clean"

# Clean repo
error_output=$(rewatch clean 2>&1)
if [ $? -eq 0 ];
then
  success "Repo Cleaned"
else
  error "Error Cleaning Repo"
  printf "%s\n" "$error_output" >&2
  exit 1
fi

# Rebuild with snapshot output
snapshot_file=../tests/snapshots/clean-rebuild.txt
rewatch build &> $snapshot_file
build_status=$?
normalize_paths $snapshot_file
if [ $build_status -eq 0 ];
then
  success "Repo Built"
else
  error "Error Building Repo"
  cat $snapshot_file >&2
  exit 1
fi

# Verify the undesired message is NOT present
if grep -q "Cleaned previous build due to compiler update" $snapshot_file; then
  error "Unexpected compiler-update clean message present in rebuild logs"
  cat $snapshot_file >&2
  exit 1
else
  success "No compiler-update clean message present after explicit clean"
fi
