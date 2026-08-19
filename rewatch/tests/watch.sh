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

# Start watcher and capture logs for debugging
rewatch_bg watch > rewatch.log 2>&1 &
success "Watcher Started"

# Trigger a recompilation
echo 'Js.log("added-by-test")' >> ./packages/main/src/Main.res

# Wait for the compiled JS to show up (can be slow in CI)
target=./packages/main/src/Main.mjs
if ! wait_for_file "$target" 20; then
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

bold "Test: Stored warnings are replayed after an early compile error"
warning_count=$(grep -c "unusedValue" rewatch.log || true)
echo 'B.world()' >> ./packages/watch-warnings/src/ModuleA.res
timeout=20
while [ "$(grep -c "unusedValue" rewatch.log || true)" -le "$warning_count" ] && [ "$timeout" -gt 0 ]; do
  sleep 1
  timeout=$((timeout - 1))
done
if [ "$timeout" -eq 0 ]; then
  error "Expected warning was not emitted before the error test"
  git checkout -- ./packages/watch-warnings/src/ModuleA.res
  exit_watcher
  exit 1
fi

error_log_start=$(($(wc -l < rewatch.log) + 1))
echo 'let broken: int = "broken"' >> ./packages/watch-warnings/src/B.res
timeout=20
while ! tail -n +"$error_log_start" rewatch.log | grep -q 'let broken' && [ "$timeout" -gt 0 ]; do
  sleep 1
  timeout=$((timeout - 1))
done
warning_replay_output=$(tail -n +"$error_log_start" rewatch.log)
if [[ "$warning_replay_output" == *"unusedValue"* ]]; then
  success "Stored warning was replayed"
else
  error "Stored warning was not replayed"
  printf "%s\n" "$warning_replay_output" >&2
  git checkout -- ./packages/watch-warnings/src/ModuleA.res ./packages/watch-warnings/src/B.res
  exit_watcher
  exit 1
fi
git checkout -- ./packages/watch-warnings/src/ModuleA.res ./packages/watch-warnings/src/B.res

sleep 1

replace '/Js.log("added-by-test")/d' ./packages/main/src/Main.res;

sleep 5

if git diff --exit-code ./
then
  success "Adding and removing changes nothing"
else
  error "Adding and removing changes left some artifacts"
  exit_watcher
  exit 1
fi

exit_watcher
