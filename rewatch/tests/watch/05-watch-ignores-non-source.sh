#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: Watcher ignores changes outside source dirs"

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

wait_for_pattern() {
  local file="$1"; local pattern="$2"; local timeout="${3:-30}"
  while [ "$timeout" -gt 0 ]; do
    grep -q "$pattern" "$file" 2>/dev/null && return 0
    sleep 1
    timeout=$((timeout - 1))
  done
  return 1
}

# Start watcher and capture output
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

sleep 1

# Record the timestamp of a compiled file
BEFORE_MTIME=$(stat -f %m ./src/Test.mjs 2>/dev/null || stat -c %Y ./src/Test.mjs 2>/dev/null)

# Create a .res file OUTSIDE any source directory (in the project root)
cat > ./NotInSourceDir.res << 'EOF'
let x = 42
EOF

# Also create a file in a random non-source subdirectory
mkdir -p ./random-dir
cat > ./random-dir/AlsoNotSource.res << 'EOF'
let y = 99
EOF

# Wait a bit to give the watcher time to (not) react
sleep 5

# Verify these files were NOT compiled
if [ -f ./NotInSourceDir.mjs ]; then
  error "File outside source dir was compiled (should be ignored)"
  rm -f ./NotInSourceDir.res ./NotInSourceDir.mjs
  rm -rf ./random-dir
  exit_watcher
  exit 1
fi
success "File in project root was correctly ignored"

if [ -f ./random-dir/AlsoNotSource.mjs ]; then
  error "File in non-source subdir was compiled (should be ignored)"
  rm -f ./NotInSourceDir.res
  rm -rf ./random-dir
  exit_watcher
  exit 1
fi
success "File in non-source subdir was correctly ignored"

# Verify no recompilation happened (mtime of existing output unchanged)
AFTER_MTIME=$(stat -f %m ./src/Test.mjs 2>/dev/null || stat -c %Y ./src/Test.mjs 2>/dev/null)
if [ "$BEFORE_MTIME" = "$AFTER_MTIME" ]; then
  success "No unnecessary recompilation triggered"
else
  error "Unexpected recompilation was triggered"
  rm -f ./NotInSourceDir.res
  rm -rf ./random-dir
  exit_watcher
  exit 1
fi

# Clean up
rm -f ./NotInSourceDir.res
rm -rf ./random-dir

exit_watcher

sleep 2
rm -f rewatch.log
