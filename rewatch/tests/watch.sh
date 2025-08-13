source "./utils.sh"
cd ../testrepo

bold "Test: It should watch"

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

# Wait until a file exists (with timeout in seconds, default 30)
wait_for_file() {
  local file="$1"; local timeout="${2:-30}"
  while [ "$timeout" -gt 0 ]; do
    [ -f "$file" ] && return 0
    sleep 1
    timeout=$((timeout - 1))
  done
  return 1
}

# Start watcher and capture logs for debugging
rewatch_bg watch > rewatch.log 2>&1 &
success "Watcher Started"

# Trigger a recompilation
echo 'Js.log("added-by-test")' >> ./packages/main/src/Main.res

# Wait for the compiled JS to show up (Windows CI can be slower)
target=./packages/main/src/Main.mjs
if ! wait_for_file "$target" 10; then
  error "Expected output not found: $target"
  ls -la ./packages/main/src || true
  tail -n 200 rewatch.log || true
  exit_watcher
  exit 1
fi

if node ./packages/main/src/Main.mjs | grep 'added-by-test' &> /dev/null;
then
  success "Output is correct"
else
  error "Output is incorrect"
  exit_watcher
  exit 1
fi

sleep 1

replace '/Js.log("added-by-test")/d' ./packages/main/src/Main.res;

sleep 1

if git diff --exit-code ./
then
  success "Adding and removing changes nothing"
else
  error "Adding and removing changes left some artifacts"
  exit_watcher
  exit 1
fi

exit_watcher