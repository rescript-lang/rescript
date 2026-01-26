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

# Create .res files in subdirectories that are NOT source dirs.
mkdir -p ./random-dir
cat > ./random-dir/NotSource.res << 'EOF'
let x = 42
EOF

mkdir -p ./another-dir/nested
cat > ./another-dir/nested/AlsoNotSource.res << 'EOF'
let y = 99
EOF

# Now trigger a real source change to prove the watcher is alive
cat > ./src/WatchProbe.res << 'EOF'
let probe = "watcher-is-alive"
EOF

# Wait for the probe file to be compiled (proves watcher is responsive)
if ! wait_for_file "./src/WatchProbe.mjs" 20; then
  error "Watcher did not respond to source change (probe file not compiled)"
  cat rewatch.log
  rm -rf ./random-dir ./another-dir ./src/WatchProbe.res
  exit_watcher
  exit 1
fi
success "Watcher responded to source change"

# Now verify the non-source files were NOT compiled
if [ -f ./random-dir/NotSource.mjs ]; then
  error "File in non-source subdir was compiled (should be ignored)"
  rm -rf ./random-dir ./another-dir ./src/WatchProbe.res ./src/WatchProbe.mjs
  exit_watcher
  exit 1
fi
success "File in non-source subdir was correctly ignored"

if [ -f ./another-dir/nested/AlsoNotSource.mjs ]; then
  error "File in nested non-source subdir was compiled (should be ignored)"
  rm -rf ./random-dir ./another-dir ./src/WatchProbe.res ./src/WatchProbe.mjs
  exit_watcher
  exit 1
fi
success "File in nested non-source subdir was correctly ignored"

# Clean up
rm -f ./src/WatchProbe.res ./src/WatchProbe.mjs
rm -rf ./random-dir ./another-dir

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
