#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: It should clean dependencies from node_modules"

# Build a package with external dependencies
error_output=$(cd packages/with-dev-deps && "$REWATCH_EXECUTABLE" build 2>&1)
if [ $? -eq 0 ];
then
  success "Built with-dev-deps"
else
  error "Error building with-dev-deps"
  printf "%s\n" "$error_output" >&2
  exit 1
fi

# Then we clean a single project
error_output=$(cd packages/with-dev-deps && "$REWATCH_EXECUTABLE" clean 2>&1)
clean_status=$?
if [ $clean_status -ne 0 ];
then
  error "Error cleaning with-dev-deps"
  printf "%s\n" "$error_output" >&2
  exit 1
fi

# Count compiled files in the cleaned project
compiler_assets=$(find node_modules/rescript-nodejs/lib/ocaml -type f -name '*.*' | wc -l | tr -d '[:space:]')
if [ $compiler_assets -eq 0 ];
then
  success "compiler assets from node_modules cleaned"
  git restore .
else
  error "Expected 0 files in node_modules/rescript-nodejs/lib/ocaml after clean, got $compiler_assets"
  printf "%s\n" "$error_output"
  exit 1
fi
