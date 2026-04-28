#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: Should clean dev-dependencies of monorepo"

# First we build the entire monorepo
error_output=$(rewatch build 2>&1)
if [ $? -eq 0 ];
then
  success "Built monorepo"
else
  error "Error building monorepo"
  printf "%s\n" "$error_output" >&2
  exit 1
fi

# Clean entire monorepo
error_output=$(rewatch clean 2>&1)
clean_status=$?
if [ $clean_status -ne 0 ];
then
  error "Error cleaning monorepo"
  printf "%s\n" "$error_output" >&2
  exit 1
fi

# Count compiled files in dev-dependency project "pure-dev"
project_compiled_files=$(find packages/pure-dev -type f -name '*.mjs' | wc -l | tr -d '[:space:]')
if [ "$project_compiled_files" -eq 0 ];
then
  success "pure-dev cleaned"
  git restore .
else
  error "Expected 0 .mjs files in pure-dev after clean, got $project_compiled_files"
  printf "%s\n" "$error_output"
  exit 1
fi
