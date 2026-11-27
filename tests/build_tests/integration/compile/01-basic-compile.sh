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

if git diff --exit-code ./;
then
  success "Testrepo has no changes"
else
  error "Build has changed"
  exit 1
fi
