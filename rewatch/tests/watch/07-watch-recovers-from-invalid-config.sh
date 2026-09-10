#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: Watcher recovers from an invalid config change"

wait_for_pattern() {
  local file="$1" pattern="$2" timeout="${3:-30}"
  while [ "$timeout" -gt 0 ]; do
    grep -q "$pattern" "$file" 2> /dev/null && return 0
    sleep 1
    timeout=$((timeout - 1))
  done
  return 1
}

wait_for_pattern_count() {
  local file="$1" pattern="$2" expected="$3" timeout="${4:-30}"
  while [ "$timeout" -gt 0 ]; do
    local count
    count=$(grep -c "$pattern" "$file" 2> /dev/null || true)
    [ "$count" -ge "$expected" ] && return 0
    sleep 1
    timeout=$((timeout - 1))
  done
  return 1
}

rewatch clean > /dev/null 2>&1
cp rescript.json rescript.json.watch-recovery.bak
rewatch_bg watch > rewatch.log 2>&1 &

if ! wait_for_file "./src/Test.mjs" 20 || ! wait_for_file "lib/watch.lock" 20; then
  error "Initial watch build did not complete"
  cat rewatch.log
  mv rescript.json.watch-recovery.bak rescript.json
  exit_watcher
  exit 1
fi

initial_completed=$(grep -c "Finished compilation" rewatch.log 2> /dev/null || true)
printf '{"name":' > rescript.json

if ! wait_for_pattern rewatch.log "Could not initialize build" 30; then
  error "Watcher did not report the invalid config"
  cat rewatch.log
  mv rescript.json.watch-recovery.bak rescript.json
  exit_watcher
  exit 1
fi

watcher_pid=$(cat lib/watch.lock)
if ! kill -0 "$watcher_pid" 2> /dev/null; then
  error "Watcher exited after the invalid config"
  cat rewatch.log
  mv rescript.json.watch-recovery.bak rescript.json
  exit 1
fi

node -e '
  const fs = require("fs");
  const config = JSON.parse(fs.readFileSync(process.argv[1], "utf8"));
  config.suffix = ".recovered.mjs";
  fs.writeFileSync(process.argv[2], `${JSON.stringify(config, null, 2)}\n`);
' rescript.json.watch-recovery.bak rescript.json

expected_completed=$((initial_completed + 1))
if ! wait_for_pattern_count rewatch.log "Finished compilation" "$expected_completed" 30; then
  error "Watcher did not recover after restoring a valid config"
  cat rewatch.log
  mv rescript.json.watch-recovery.bak rescript.json
  exit_watcher
  exit 1
fi

echo '// watch-recovery-test' >> src/Test.res
if ! wait_for_file "./src/Test.recovered.mjs" 20; then
  error "Recovered watcher did not compile with the restored config"
  cat rewatch.log
  mv rescript.json.watch-recovery.bak rescript.json
  git checkout -- src/Test.res
  exit_watcher
  exit 1
fi

exit_watcher
rewatch clean > /dev/null 2>&1
mv rescript.json.watch-recovery.bak rescript.json
git checkout -- src/Test.res
rm -f rewatch.log
rewatch build > /dev/null 2>&1

if git diff --exit-code . > /dev/null 2>&1 && [ -z "$(git ls-files --others --exclude-standard .)" ]; then
  success "Watcher recovered from invalid config"
else
  error "Watcher recovery test left repository changes"
  git diff .
  git ls-files --others --exclude-standard .
  exit 1
fi
