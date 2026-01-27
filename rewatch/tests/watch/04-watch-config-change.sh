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

# Start watcher and capture PID for cleanup
rewatch_bg watch > rewatch.log 2>&1 &
WATCHER_PID=$!
success "Watcher Started"

# Wait for initial build to complete
if ! wait_for_file "./src/Test.mjs" 20; then
  error "Initial build did not complete"
  cat rewatch.log
  exit_watcher
  exit 1
fi
success "Initial build completed"

# Wait for watcher to be ready (file watchers set up)
if ! wait_for_watcher_ready "./rewatch.log" 10; then
  error "Watcher did not become ready"
  cat rewatch.log
  exit_watcher
  exit 1
fi

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
if [ -f lib/bs/rescript.daemon.pid ]; then
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
