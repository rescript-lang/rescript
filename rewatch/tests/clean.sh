#!/bin/bash
cd $(dirname $0)
source "./utils.sh"
cd ../testrepo

bold "Test: It should clean a single project"

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

# Then we clean a single project
error_output=$(cd packages/file-casing && "../../$REWATCH_EXECUTABLE" clean 2>&1)
clean_status=$?
if [ $clean_status -ne 0 ];
then
  error "Error cleaning current project file-casing"
  printf "%s\n" "$error_output" >&2
  exit 1
fi

# Count compiled files in the cleaned project
project_compiled_files=$(find packages/file-casing -type f -name '*.mjs' | wc -l | tr -d '[:space:]')
if [ "$project_compiled_files" -eq 0 ];
then
  success "file-casing cleaned"
else
  error "Expected 0 .mjs files in file-casing after clean, got $project_compiled_files"
  printf "%s\n" "$error_output"
  exit 1
fi

# Ensure other project files were not cleaned
other_project_compiled_files=$(find packages/new-namespace -type f -name '*.mjs' | wc -l | tr -d '[:space:]')
if [ "$other_project_compiled_files" -gt 0 ];
then
  success "Didn't clean other project files"
  git restore .
else
  error "Expected files from new-namespace not to be cleaned"
  exit 1
fi

bold "--dev should clean dev-dependencies of monorepo"

# First we build the entire monorepo (including --dev)
error_output=$(rewatch build --dev 2>&1)
if [ $? -eq 0 ];
then
  success "Built monorepo"
else
  error "Error building monorepo"
  printf "%s\n" "$error_output" >&2
  exit 1
fi

# Clean entire monorepo (including --dev)
error_output=$(rewatch clean --dev 2>&1)
clean_status=$?
if [ $clean_status -ne 0 ];
then
  error "Error cleaning current project file-casing"
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

bold "Test: It should clean dependencies from node_modules"

# Build a package with external dependencies
error_output=$(cd packages/with-dev-deps && "../../$REWATCH_EXECUTABLE" build 2>&1)
if [ $? -eq 0 ];
then
  success "Built with-dev-deps"
else
  error "Error building with-dev-deps"
  printf "%s\n" "$error_output" >&2
  exit 1
fi

# Then we clean a single project
error_output=$(cd packages/with-dev-deps && "../../$REWATCH_EXECUTABLE" clean 2>&1)
clean_status=$?
if [ $clean_status -ne 0 ];
then
  error "Error cleaning current project file-casing"
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