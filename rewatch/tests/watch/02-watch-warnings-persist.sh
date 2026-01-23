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

exit_watcher() {
  # kill watcher by removing lock file
  rm lib/rescript.lock
}

# Start watcher and capture stderr (where warnings are printed)
rewatch_bg watch > /dev/null 2> rewatch-stderr.log &
sleep 8
success "Watcher Started"

# Verify the initial build produces the warning from ModuleA.res
if grep -q "unused value unusedValue" rewatch-stderr.log;
then
  success "Initial build shows warning from ModuleA.res"
else
  error "Initial build does not show warning from ModuleA.res"
  cat rewatch-stderr.log
  exit_watcher
  exit 1
fi

# Trigger a recompilation of B.res only
printf '// trigger recompile\n' >> ./packages/watch-warnings/src/B.res

sleep 5

# Check that the warning from ModuleA.res is still present after incremental build
# Count occurrences: should be 2 (initial + incremental)
warning_count=$(grep -c "unused value unusedValue" rewatch-stderr.log)
if [ "$warning_count" -ge 2 ];
then
  success "Warning from ModuleA.res persists after recompiling B.res (count: $warning_count)"
else
  error "Warning from ModuleA.res was lost after recompiling B.res (count: $warning_count)"
  cat rewatch-stderr.log
  exit_watcher
  printf 'let world = () => Console.log("world")\n' > ./packages/watch-warnings/src/B.res
  exit 1
fi

# Restore B.res
printf 'let world = () => Console.log("world")\n' > ./packages/watch-warnings/src/B.res

sleep 1

exit_watcher

sleep 2

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
