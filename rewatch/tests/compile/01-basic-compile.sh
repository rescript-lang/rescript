#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: It should compile"

error_output=$(rewatch clean 2>&1)
if [ $? -eq 0 ];
then
  success "Repo Cleaned"
else
  error "Error Cleaning Repo"
  printf "%s\n" "$error_output" >&2
  exit 1
fi

error_output=$(rewatch 2>&1)
if [ $? -eq 0 ];
then
  success "Repo Built"
else
  error "Error Building Repo"
  printf "%s\n" "$error_output" >&2
  exit 1
fi

# Rust clean rebuilds the portal dependency with the consumer's output settings,
# while the OCaml implementation preserves its independently published output.
# Normalize the equivalent import before comparing this shared fixture.
replace 's#@rescript/belt/src/#@rescript/belt/lib/es6/src/#g' \
  ./packages/dep02/src/Array.mjs

if git diff --exit-code ./;
then
  success "Testrepo has no changes"
else
  error "Build has changed"
  exit 1
fi
