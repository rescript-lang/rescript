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

# Wait until a pattern appears in a file (with timeout in seconds, default 30)
wait_for_pattern() {
  local file="$1"; local pattern="$2"; local timeout="${3:-30}"
  while [ "$timeout" -gt 0 ]; do
    grep -q "$pattern" "$file" 2>/dev/null && return 0
    sleep 1
    timeout=$((timeout - 1))
  done
  return 1
}

# Wait until a pattern appears N times in a file (with timeout in seconds, default 30)
wait_for_pattern_count() {
  local file="$1"; local pattern="$2"; local count="$3"; local timeout="${4:-30}"
  while [ "$timeout" -gt 0 ]; do
    local current_count=$(grep -c "$pattern" "$file" 2>/dev/null || echo "0")
    [ "$current_count" -ge "$count" ] && return 0
    sleep 1
    timeout=$((timeout - 1))
  done
  return 1
}

# Start watcher and capture output (stdout to log, stderr to separate file for warnings)
rewatch_bg watch > rewatch.log 2> rewatch-stderr.log &
WATCHER_PID=$!

# Wait for watcher to be ready (file watchers set up)
if ! wait_for_watcher_ready "./rewatch.log" 30; then
  error "Watcher did not become ready"
  cat rewatch.log
  cat rewatch-stderr.log
  exit_watcher
  exit 1
fi

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
printf 'let world = () => Console.log("world")\n' > ./packages/watch-warnings/src/B.res

sleep 1

if exit_watcher; then
  success "Daemon shut down cleanly"
else
  rm -f rewatch-stderr.log
  exit 1
fi

# Clean up log files
rm -f rewatch.log rewatch-stderr.log

# Verify no leftover changes
if git diff --exit-code ./packages/watch-warnings > /dev/null 2>&1;
then
  success "No leftover changes in watch-warnings package"
else
  error "Leftover changes detected in watch-warnings package"
  git diff ./packages/watch-warnings
  exit 1
fi
