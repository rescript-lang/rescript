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

if ! wait_for_pattern_count rewatch.log "Finished .*compilation" 1 30; then
  error "Initial build did not settle"
  cat rewatch.log
  exit_watcher
  exit 1
fi
completed_builds=$(grep -c "Finished .*compilation" rewatch.log 2>/dev/null || true)
completed_builds=${completed_builds:-0}
wait_for_next_build() {
  completed_builds=$((completed_builds + 1))
  wait_for_pattern_count rewatch.log "Finished .*compilation" \
    "$completed_builds" 30
}

wait_for_tracked_outputs() {
  local timeout
  timeout=$(platform_timeout 30)
  while [ "$timeout" -gt 0 ]; do
    if [ -z "$(git ls-files --deleted -- '*.mjs')" ]; then
      return 0
    fi
    sleep 1
    timeout=$((timeout - 1))
  done
  return 1
}

# Change the suffix in rescript.json (same approach as suffix test)
replace "s/.mjs/.res.mjs/g" rescript.json
if ! wait_for_next_build; then
  error "Configuration rebuild did not settle"
  cat rewatch.log
  replace "s/.res.mjs/.mjs/g" rescript.json
  exit_watcher
  exit 1
fi

# After a config change, the watcher does a full rebuild. However, a suffix
# change alone may not recompile files (sources haven't changed). Trigger a
# source change so the watcher compiles with the new suffix.
echo '// config-change-test' >> ./src/Test.res

# Wait for the file with the new suffix and the complete source rebuild.
if wait_for_file "./src/Test.res.mjs" 20 && wait_for_next_build; then
  success "Full rebuild triggered by rescript.json change (new suffix applied)"
else
  error "No rebuild detected after rescript.json change"
  cat rewatch.log
  replace "s/.res.mjs/.mjs/g" rescript.json
  restore_tracked_files ./src/Test.res
  exit_watcher
  exit 1
fi

# Verify the watcher is still running (didn't crash on config change)
if [ -f lib/watch.lock ]; then
  success "Watcher still running after config change"
else
  error "Watcher crashed after config change"
  cat rewatch.log
  replace "s/.res.mjs/.mjs/g" rescript.json
  restore_tracked_files ./src/Test.res
  exit 1
fi

# Restore only the configuration while the watcher is running. Mixing the
# unrelated source cleanup into the same native event batch can make the batch
# look like an incremental source edit and obscure the configuration transition
# this test is intended to verify.
replace "s/.res.mjs/.mjs/g" rescript.json
if wait_for_next_build && wait_for_tracked_outputs; then
  success "Rebuild after configuration restore completed"
else
  error "Configuration restore did not settle or restore tracked outputs"
  restore_tracked_files ./src/Test.res
  exit_watcher
  exit 1
fi

# This fixture verifies watcher reconfiguration. Stale output migration has its
# own implementation-specific regression coverage.
find . -name "*.res.mjs" -delete 2>/dev/null

if ! exit_watcher; then
  exit 1
fi

restore_tracked_files ./src/Test.res
sleep 2
rm -f rewatch.log

normalize_belt_portal_import

if git diff --exit-code . > /dev/null 2>&1 && [ -z "$(git ls-files --others --exclude-standard .)" ];
then
  success "No leftover changes"
else
  error "Leftover changes detected"
  git diff .
  git ls-files --others --exclude-standard .
  exit 1
fi
