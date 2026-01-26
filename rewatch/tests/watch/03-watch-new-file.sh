#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: Watcher picks up new files in source dirs"

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

# Create a new file in the source directory
cat > ./src/NewWatchTestFile.res << 'EOF'
let greeting = "hello from new file"
let () = Js.log(greeting)
EOF

# Wait for the new file to be compiled
if ! wait_for_file "./src/NewWatchTestFile.mjs" 20; then
  error "New file was not compiled by watcher"
  cat rewatch.log
  rm -f ./src/NewWatchTestFile.res
  exit_watcher
  exit 1
fi

if node ./src/NewWatchTestFile.mjs | grep 'hello from new file' &> /dev/null;
then
  success "New file was compiled correctly"
else
  error "New file output is incorrect"
  rm -f ./src/NewWatchTestFile.res ./src/NewWatchTestFile.mjs
  exit_watcher
  exit 1
fi

# Clean up the new file
rm -f ./src/NewWatchTestFile.res

# Wait for the compiled output to be removed (full rebuild detects removal)
sleep 5
rm -f ./src/NewWatchTestFile.mjs

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
