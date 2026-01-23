#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: Watcher triggers full rebuild on rescript.json change"

error_output=$(rewatch clean 2>&1)
if [ $? -eq 0 ];
then
  success "Repo Cleaned"
else
  error "Error Cleaning Repo"
  printf "%s\n" "$error_output" >&2
  exit 1
fi

exit_watcher() {
  rm -f lib/rescript.lock
}

wait_for_file() {
  local file="$1"; local timeout="${2:-30}"
  while [ "$timeout" -gt 0 ]; do
    [ -f "$file" ] && return 0
    sleep 1
    timeout=$((timeout - 1))
  done
  return 1
}

# Start watcher and capture all output
rewatch_bg watch > rewatch.log 2>&1 &
success "Watcher Started"

# Wait for initial build to complete
if ! wait_for_file "./src/Test.mjs" 20; then
  error "Initial build did not complete"
  cat rewatch.log
  exit_watcher
  exit 1
fi
success "Initial build completed"

sleep 2

# Change the suffix in rescript.json (same approach as suffix test)
replace "s/.mjs/.res.mjs/g" rescript.json

# Wait for a full rebuild to produce files with the new suffix
if wait_for_file "./src/Test.res.mjs" 20; then
  success "Full rebuild triggered by rescript.json change (new suffix applied)"
else
  error "No rebuild detected after rescript.json change"
  cat rewatch.log
  replace "s/.res.mjs/.mjs/g" rescript.json
  exit_watcher
  exit 1
fi

# Verify the watcher is still running (didn't crash on config change)
if [ -f lib/rescript.lock ]; then
  success "Watcher still running after config change"
else
  error "Watcher crashed after config change"
  cat rewatch.log
  replace "s/.res.mjs/.mjs/g" rescript.json
  exit 1
fi

# Restore rescript.json
replace "s/.res.mjs/.mjs/g" rescript.json

# Wait for rebuild with restored suffix
sleep 8

exit_watcher

sleep 2

# Clean up generated .res.mjs files if any remain
find . -name "*.res.mjs" -delete 2>/dev/null

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
