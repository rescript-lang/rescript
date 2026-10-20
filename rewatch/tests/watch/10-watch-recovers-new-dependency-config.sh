#!/bin/bash
set -e
cd "$(dirname "$0")"
source "../utils.sh"

bold "Test: Watcher recovers when only a newly added dependency config is fixed"

test_dir=$(pwd)
fixture=$(mktemp -d)
cleanup() {
  cd "$fixture" || return 1
  if ! exit_watcher; then
    return 1
  fi
  if [ -n "${watch_pid:-}" ]; then
    if process_is_running "$watch_pid"; then
      kill "$watch_pid" 2>/dev/null || true
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

node <<'JS'
const fs = require("fs");
const path = require("path");
const config = name => ({
  name,
  sources: ["src"],
  "package-specs": {module: "esmodule", "in-source": true},
  suffix: ".mjs",
});
fs.mkdirSync("src");
fs.writeFileSync("src/Main.res", "let value = 0\n");
fs.writeFileSync("rescript.json", JSON.stringify(config("watch-dependency")));
fs.mkdirSync("packages/new-dependency/src", {recursive: true});
fs.writeFileSync("packages/new-dependency/src/Dep.res", "let value = 1\n");
fs.writeFileSync("packages/new-dependency/package.json", JSON.stringify({name: "new-dependency", version: "1.0.0"}));
fs.writeFileSync("valid-dependency.json", JSON.stringify(config("new-dependency")));
fs.writeFileSync("packages/new-dependency/rescript.json", '{"name":');
fs.mkdirSync("node_modules");
fs.symlinkSync(path.resolve("packages/new-dependency"), "node_modules/new-dependency", "junction");
JS

rewatch watch > rewatch.log 2>&1 &
watch_pid=$!
wait_for_pattern rewatch.log "Finished initial compilation" 60 ||
  fail_test "Initial watch build did not finish"
completed=$(grep -c "Finished compilation" rewatch.log || true)

node <<'JS'
const fs = require("fs");
const config = JSON.parse(fs.readFileSync("rescript.json", "utf8"));
config.dependencies = ["new-dependency"];
fs.writeFileSync("rescript.next", JSON.stringify(config));
JS
mv rescript.next rescript.json
wait_for_pattern rewatch.log "Could not build package tree for 'new-dependency'" 30 ||
  fail_test "Watcher did not report the newly added dependency's invalid config"

# Only fix the dependency: its directory must have a recovery watch by now.
cp valid-dependency.json packages/new-dependency/rescript.next
mv packages/new-dependency/rescript.next packages/new-dependency/rescript.json
wait_for_pattern_count rewatch.log "Finished compilation" "$((completed + 1))" 30 ||
  fail_test "Watcher did not recover after fixing only the dependency config"
wait_for_file packages/new-dependency/src/Dep.mjs 20 ||
  fail_test "Recovered watcher did not compile the new dependency"

success "Watcher recovered after only the new dependency config was fixed"
