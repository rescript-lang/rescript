#!/bin/bash
set -e
cd "$(dirname "$0")"
source "../utils.sh"

bold "Test: Watcher recovers from invalid JSON and JSX config changes"

test_dir=$(pwd)
fixture=$(mktemp -d)
cleanup() {
  cd "$fixture" || return 1
  if ! exit_watcher; then
    return 1
  fi
  if [ -n "${watch_pid:-}" ]; then
    if process_is_running "$watch_pid"; then
      kill "$watch_pid"
    fi
    wait "$watch_pid" 2>/dev/null || true
  fi
  cd "$test_dir"
  rm -rf "$fixture"
}
trap cleanup EXIT
cd "$fixture"

fail_test() {
  error "$1"
  cat rewatch.log >&2
  exit 1
}

mkdir src
printf 'let value = 0\n' > src/Test.res
cat > rescript.json <<'JSON'
{
  "name": "watch-recovery",
  "sources": ["src"],
  "package-specs": {"module": "esmodule", "in-source": true},
  "suffix": ".mjs",
  "js-post-build": {"cmd": "node record-output.cjs"}
}
JSON
cp rescript.json valid.json
cat > record-output.cjs <<'JS'
const fs = require("fs");
const path = require("path");
fs.appendFileSync("outputs.log", path.basename(process.argv[2]) + "\n");
JS

rewatch watch > rewatch.log 2>&1 &
watch_pid=$!
wait_for_pattern rewatch.log "Finished initial compilation" 60 ||
  fail_test "Initial watch build did not finish"

previous_output='Test.mjs'
iteration=0
for invalid_config in '{"name":' '{"name":"watch-recovery","jsx":{"version":3}}'; do
  iteration=$((iteration + 1))
  completed=$(grep -c "Finished compilation" rewatch.log || true)
  errors=$(grep -c 'Could not initialize build\|invalid JSON\|jsx.version' rewatch.log || true)
  printf '%s\n' "$invalid_config" > rescript.next
  mv rescript.next rescript.json
  wait_for_pattern_count rewatch.log 'Could not initialize build\|invalid JSON\|jsx.version' "$((errors + 1))" 30 ||
    fail_test "Watcher did not report the invalid config"

  # Record emitted files across recovery, even if a later cleanup removes them.
  # Both CLIs must leave the old output untouched while the config is invalid.
  : > outputs.log
  printf 'let marker%s = "watch-recovery-marker-%s"\n' "$iteration" "$iteration" >> src/Test.res
  if wait_for_pattern outputs.log "$previous_output" 5; then
    fail_test "Watcher compiled the source using stale configuration"
  fi
  process_is_running "$watch_pid" || fail_test "Watcher exited after the config error"

  node - "$iteration" <<'JS'
const fs = require("fs");
const config = JSON.parse(fs.readFileSync("valid.json", "utf8"));
config.suffix = `.recovered${process.argv[2]}.mjs`;
fs.writeFileSync("rescript.next", JSON.stringify(config));
JS
  mv rescript.next rescript.json
  wait_for_pattern_count rewatch.log "Finished compilation" "$((completed + 1))" 30 ||
    fail_test "Watcher did not recover after restoring a valid config"
  wait_for_pattern "src/Test.recovered$iteration.mjs" "watch-recovery-marker-$iteration" 20 ||
    fail_test "Recovered watcher did not compile the edited source with the new suffix"
  if grep -Fq "$previous_output" outputs.log; then
    fail_test "Watcher emitted stale output before recovering"
  fi
  wait_for_file_gone lib/build.lock 20 || fail_test "Recovery left the build lock behind"
  previous_output="Test.recovered$iteration.mjs"
done

success "Both config errors recover without compiling against stale configuration"
