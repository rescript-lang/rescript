#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: Warnings from non-recompiled modules persist in watch mode"

error_output=$(rewatch clean 2>&1)
if [ $? -eq 0 ];
then
  success "Repo Cleaned"
else
  error "Error Cleaning Repo"
  printf "%s\n" "$error_output" >&2
  exit 1
fi

# Start watcher and capture stderr (where warnings are printed)
rewatch_bg watch > /dev/null 2> rewatch-stderr.log &

# Wait for initial compilation to produce the warning
if ! wait_for_pattern rewatch-stderr.log "unused value unusedValue" 30; then
  error "Initial build does not show warning from ModuleA.res"
  cat rewatch-stderr.log
  exit_watcher
  exit 1
fi
success "Initial build shows warning from ModuleA.res"

# Trigger a recompilation of B.res only
printf '// trigger recompile\n' >> ./packages/watch-warnings/src/B.res

# Wait for the warning to appear a second time (from the incremental build)
if ! wait_for_pattern_count rewatch-stderr.log "unused value unusedValue" 2 20; then
  warning_count=$(grep -c "unused value unusedValue" rewatch-stderr.log || echo "0")
  error "Warning from ModuleA.res was lost after recompiling B.res (count: $warning_count)"
  cat rewatch-stderr.log
  exit_watcher
  printf 'let world = () => Console.log("world")\n' > ./packages/watch-warnings/src/B.res
  exit 1
fi

warning_count=$(grep -c "unused value unusedValue" rewatch-stderr.log)
success "Warning from ModuleA.res persists after recompiling B.res (count: $warning_count)"

# Restore B.res
warning_count_before_restore=$(grep -c "unused value unusedValue" rewatch-stderr.log 2>/dev/null || true)
warning_count_before_restore=${warning_count_before_restore:-0}
printf 'let world = () => Console.log("world")\n' > ./packages/watch-warnings/src/B.res

if ! wait_for_pattern_count rewatch-stderr.log "unused value unusedValue" \
  "$((warning_count_before_restore + 1))" 20; then
  error "Watcher did not finish rebuilding after restoring B.res"
  cat rewatch-stderr.log
  exit_watcher
  exit 1
fi

if ! exit_watcher; then
  exit 1
fi

# A new invocation has no retained in-memory warning state, so the watcher must
# leave a freshness marker that causes the warning-producing module to run.
next_output=$(rewatch build 2>&1)
next_status=$?
if [ "$next_status" -eq 0 ] \
  && printf '%s\n' "$next_output" | grep -q "unused value unusedValue"; then
  success "Warning persists after watcher shutdown"
else
  error "Warning was lost after watcher shutdown"
  printf '%s\n' "$next_output"
  exit 1
fi

# Clean up log file
rm -f rewatch-stderr.log

# Verify no leftover changes
if git diff --exit-code ./packages/watch-warnings > /dev/null 2>&1;
then
  success "No leftover changes in watch-warnings package"
else
  error "Leftover changes detected in watch-warnings package"
  git diff ./packages/watch-warnings
  exit 1
fi
