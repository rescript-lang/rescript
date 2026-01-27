#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: Watcher reports missing source folders"

error_output=$(rewatch clean 2>&1)
if [ $? -eq 0 ];
then
  success "Repo Cleaned"
else
  error "Error Cleaning Repo"
  printf "%s\n" "$error_output" >&2
  exit 1
fi

DEP01_CONFIG=packages/dep01/rescript.json

# Add a non-existent source folder to dep01's rescript.json
node -e "
  const fs = require('fs');
  const config = JSON.parse(fs.readFileSync('$DEP01_CONFIG', 'utf8'));
  config.sources = [{dir: 'nonexistent-folder'}, config.sources];
  fs.writeFileSync('$DEP01_CONFIG', JSON.stringify(config, null, 2) + '\n');
"

# Start watcher and capture PID for cleanup
rewatch_bg watch > rewatch.log 2>&1 &
WATCHER_PID=$!
success "Watcher Started"

# Wait for initial build to complete
if ! wait_for_file "./src/Test.mjs" 20; then
  error "Initial build did not complete"
  cat rewatch.log
  git checkout "$DEP01_CONFIG"
  exit_watcher
  exit 1
fi
success "Initial build completed"

# Wait for watcher to be ready (file watchers set up)
if ! wait_for_watcher_ready "./rewatch.log" 10; then
  error "Watcher did not become ready"
  cat rewatch.log
  git checkout "$DEP01_CONFIG"
  exit_watcher
  exit 1
fi

# Check that the warning about the missing folder was logged
if grep -q 'could not watch.*nonexistent-folder' rewatch.log; then
  success "Missing source folder warning was reported"
else
  error "Missing source folder warning was NOT reported"
  cat rewatch.log
  git checkout "$DEP01_CONFIG"
  exit_watcher
  exit 1
fi

# Exit the watcher before restoring the config to avoid a race condition
# where the config change triggers a full rebuild that runs concurrently
# with the subsequent `rewatch build`.
exit_watcher
sleep 1

# Restore dep01's rescript.json
git checkout "$DEP01_CONFIG"

# Rebuild to regenerate any artifacts that were removed by `rewatch clean`
# but not rebuilt due to the modified config (e.g. Dep01.mjs).
rewatch build > /dev/null 2>&1
rm -f rewatch.log

if git diff --exit-code . > /dev/null 2>&1 && [ -z "$(git ls-files --others --exclude-standard .)" ];
then
  success "No leftover changes"
else
  error "Leftover changes detected"
  git diff .
  git ls-files --others --exclude-standard .
  exit 1
fi
