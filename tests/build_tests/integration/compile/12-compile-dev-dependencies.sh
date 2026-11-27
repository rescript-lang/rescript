#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: It should compile dev dependencies"

rewatch clean &> /dev/null
rewatch build &> /dev/null
if [ $? -ne 0 ];
then
  error "Failed to compile dev dependencies"
  exit 1
fi

file_count=$(find ./packages/with-dev-deps/test -name *.mjs | wc -l)
expected_file_count=1
if [ "$file_count" -eq $expected_file_count ];
then
  success "Compiled dev dependencies successfully"
else
  error "Expected $expected_file_count files to be compiled, found $file_count"
  exit 1
fi

error_output=$(rewatch clean 2>&1 >/dev/null)
file_count=$(find ./packages/with-dev-deps -name *.mjs | wc -l)
if [ "$file_count" -eq 0 ];
then
  success "Cleaned dev dependencies successfully"
else
  error "Expected 0 files remaining after cleaning, found $file_count"
  printf "%s\n" "$error_output" >&2
  exit 1
fi
