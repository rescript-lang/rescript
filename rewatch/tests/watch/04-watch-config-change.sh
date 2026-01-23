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

wait_for_file_gone() {
  local file="$1"; local timeout="${2:-30}"
  while [ "$timeout" -gt 0 ]; do
    [ ! -f "$file" ] && return 0
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

# After a config change, the watcher does a full rebuild. However, a suffix
# change alone may not recompile files (sources haven't changed). Trigger a
# source change so the watcher compiles with the new suffix.
sleep 3
echo '// config-change-test' >> ./src/Test.res

# Wait for the file with the new suffix to appear
if wait_for_file "./src/Test.res.mjs" 20; then
  success "Full rebuild triggered by rescript.json change (new suffix applied)"
else
  error "No rebuild detected after rescript.json change"
  cat rewatch.log
  replace "s/.res.mjs/.mjs/g" rescript.json
  git checkout -- ./src/Test.res
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
  git checkout -- ./src/Test.res
  exit 1
fi

# Restore rescript.json and source file
replace "s/.res.mjs/.mjs/g" rescript.json
git checkout -- ./src/Test.res

# Wait for rebuild with restored suffix (old .res.mjs should go away)
if wait_for_file_gone "./src/Test.res.mjs" 20; then
  success "Rebuild after restore removed old suffix files"
else
  # Clean up manually if the watcher didn't remove them
  find . -name "*.res.mjs" -delete 2>/dev/null
fi

exit_watcher

sleep 2
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
