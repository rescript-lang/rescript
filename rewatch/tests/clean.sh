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

bold "-should clean dev-dependencies of monorepo"

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
rewatch build --snapshot-output &> $snapshot_file
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
