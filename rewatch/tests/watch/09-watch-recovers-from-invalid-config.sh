#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: Watcher recovers from an invalid config change"

stop_watcher() {
  if ! exit_watcher; then
    exit 1
  fi
}

wait_for_source_retry() {
  local marker='received Modify.*Test[.]res.* -> Full'
  wait_for_pattern rewatch.log "$marker" 30 || return 1
  # Require an initialization failure after the source event, not another config event before it.
  local preceding_errors
  preceding_errors=$(awk -v marker="$marker" '
    $0 ~ marker {exit}
    /Could not initialize build|invalid JSON/ {count++}
    END {print count + 0}
  ' rewatch.log)
  wait_for_pattern_count rewatch.log 'Could not initialize build\|invalid JSON' "$((preceding_errors + 1))" 30
}

if ! rewatch clean > /dev/null 2>&1; then
  error "Could not clean before watcher recovery test"
  exit 1
fi
cp rescript.json rescript.json.watch-recovery.bak
rewatch_bg -vv watch > rewatch.log 2>&1 &

if ! wait_for_file "./src/Test.mjs" 20 || ! wait_for_file "lib/watch.lock" 20; then
  error "Initial watch build did not complete"
  cat rewatch.log
  stop_watcher
  mv rescript.json.watch-recovery.bak rescript.json
  exit 1
fi

if ! wait_for_pattern rewatch.log "Finished initial compilation" 60; then
  error "Initial watch build did not finish"
  cat rewatch.log
  stop_watcher
  mv rescript.json.watch-recovery.bak rescript.json
  exit 1
fi

initial_completed=$(grep -c "Finished compilation" rewatch.log 2> /dev/null || true)
printf '{"name":' > rescript.json.watch-recovery.next
mv rescript.json.watch-recovery.next rescript.json

if ! wait_for_pattern rewatch.log 'Could not initialize build\|invalid JSON' 30; then
  error "Watcher did not report the invalid config"
  cat rewatch.log
  stop_watcher
  mv rescript.json.watch-recovery.bak rescript.json
  exit 1
fi

# A content-only edit must retry initialization instead of compiling with the old config.
echo 'let watchRecoveryMarker = "watch-recovery-marker"' >> src/Test.res
# OCaml watches only configuration while the root config is invalid.
# Rust additionally guarantees a source-triggered initialization retry.
if ! rewatch --help | grep -q '^NAME$' && ! wait_for_source_retry; then
  error "Source edit did not retry failed initialization"
  cat rewatch.log
  stop_watcher
  mv rescript.json.watch-recovery.bak rescript.json
  git restore --worktree -- src/Test.res
  exit 1
fi
if grep -q "watch-recovery-marker" src/Test.mjs 2> /dev/null; then
  error "Watcher compiled the source using stale configuration"
  stop_watcher
  mv rescript.json.watch-recovery.bak rescript.json
  git restore --worktree -- src/Test.res
  exit 1
fi

node -e '
  const fs = require("fs");
  const config = JSON.parse(fs.readFileSync(process.argv[1], "utf8"));
  config.suffix = ".recovered.mjs";
  fs.writeFileSync(process.argv[2], `${JSON.stringify(config, null, 2)}\n`);
' rescript.json.watch-recovery.bak rescript.json.watch-recovery.next
mv rescript.json.watch-recovery.next rescript.json

expected_completed=$((initial_completed + 1))
if ! wait_for_pattern_count rewatch.log "Finished compilation" "$expected_completed" 30; then
  error "Watcher did not recover after restoring a valid config"
  cat rewatch.log
  stop_watcher
  mv rescript.json.watch-recovery.bak rescript.json
  git restore --worktree -- src/Test.res
  exit 1
fi

if ! wait_for_pattern "./src/Test.recovered.mjs" "watch-recovery-marker" 20; then
  error "Recovered watcher did not compile with the restored config"
  cat rewatch.log
  stop_watcher
  mv rescript.json.watch-recovery.bak rescript.json
  git restore --worktree -- src/Test.res
  exit 1
fi

stop_watcher
if ! rewatch clean > /dev/null 2>&1; then
  error "Could not clean after watcher recovery test"
  exit 1
fi
mv rescript.json.watch-recovery.bak rescript.json
git restore --worktree -- src/Test.res
rm -f rewatch.log
if ! rewatch build > /dev/null 2>&1; then
  error "Could not rebuild after watcher recovery test"
  exit 1
fi

if git diff --exit-code . > /dev/null 2>&1 && [ -z "$(git ls-files --others --exclude-standard .)" ]; then
  success "Watcher recovered from invalid config"
else
  error "Watcher recovery test left repository changes"
  git diff .
  git ls-files --others --exclude-standard .
  exit 1
fi
