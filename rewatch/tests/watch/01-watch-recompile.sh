#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: It should watch"

error_output=$(rewatch clean 2>&1)
if [ $? -eq 0 ];
then
  success "Repo Cleaned"
else
  error "Error Cleaning Repo"
  printf "%s\n" "$error_output" >&2
  exit 1
fi

# Start watcher and capture PID for cleanup
rewatch_bg watch > rewatch.log 2>&1 &
WATCHER_PID=$!
success "Watcher Started"

# Wait for watcher to be ready (file watchers set up)
if ! wait_for_watcher_ready "./rewatch.log" 30; then
  error "Watcher did not become ready"
  cat rewatch.log
  exit_watcher
  exit 1
fi

# Trigger a recompilation
echo 'Js.log("added-by-test")' >> ./packages/main/src/Main.res

# Wait for the compiled JS to contain our new code
target=./packages/main/src/Main.mjs
if ! wait_for_file_content "$target" "added-by-test" 20; then
  error "Expected output not found in $target"
  ls -la ./packages/main/src || true
  cat "$target" 2>/dev/null || true
  tail -n 200 rewatch.log || true
  exit_watcher
  exit 1
fi
success "Output is correct"

sleep 1

replace '/Js.log("added-by-test")/d' ./packages/main/src/Main.res;

sleep 5

if git diff --exit-code ./
then
  success "Adding and removing changes nothing"
else
  error "Adding and removing changes left some artifacts"
  exit_watcher
  exit 1
fi

if exit_watcher; then
  success "Daemon shut down cleanly"
else
  exit 1
fi
