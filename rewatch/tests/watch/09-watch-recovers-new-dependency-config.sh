#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: Watcher recovers when only a newly added dependency config is fixed"

dependency_config=packages/standalone/rescript.json

stop_and_restore() {
  if ! exit_watcher; then
    return 1
  fi
  mv rescript.json.watch-dependency.bak rescript.json
  mv "$dependency_config.watch-dependency.bak" "$dependency_config"
  rm -f rewatch.log
  if ! rewatch build > /dev/null 2>&1; then
    error "Could not rebuild testrepo after dependency recovery test"
    return 1
  fi
  # The root no longer depends on standalone; restore its artifacts with its own build.
  if ! (cd packages/standalone && rewatch build > /dev/null 2>&1); then
    error "Could not rebuild standalone after dependency recovery test"
    return 1
  fi
}

fail_test() {
  error "$1"
  cat rewatch.log
  if ! stop_and_restore; then
    exit 1
  fi
  exit 1
}

if ! rewatch clean > /dev/null 2>&1; then
  error "Could not clean before dependency recovery test"
  exit 1
fi
cp rescript.json rescript.json.watch-dependency.bak
cp "$dependency_config" "$dependency_config.watch-dependency.bak"
rewatch_bg watch > rewatch.log 2>&1 &

if ! wait_for_file "lib/watch.lock" 20 || ! wait_for_pattern rewatch.log "Finished initial compilation" 60; then
  fail_test "Initial watch build did not finish"
fi
initial_completed=$(grep -c "Finished compilation" rewatch.log 2> /dev/null || true)

# Standalone is installed but absent from the root's original ReScript dependency graph.
printf '{"name":' > "$dependency_config.watch-dependency.next"
mv "$dependency_config.watch-dependency.next" "$dependency_config"
node -e '
  const fs = require("fs");
  const config = JSON.parse(fs.readFileSync(process.argv[1], "utf8"));
  config.dependencies.push("@testrepo/standalone");
  fs.writeFileSync(process.argv[2], `${JSON.stringify(config, null, 2)}\n`);
' rescript.json rescript.json.watch-dependency.next
mv rescript.json.watch-dependency.next rescript.json

if ! wait_for_pattern rewatch.log "Could not read dependency config.*standalone" 30; then
  fail_test "Watcher did not report the newly added dependency's invalid config"
fi

# Only fix the new dependency. Its directory must have a recovery watch by this point.
cp "$dependency_config.watch-dependency.bak" "$dependency_config.watch-dependency.next"
mv "$dependency_config.watch-dependency.next" "$dependency_config"
if ! wait_for_pattern_count rewatch.log "Finished compilation" "$((initial_completed + 1))" 30; then
  fail_test "Watcher did not recover after fixing only the new dependency's config"
fi

if ! stop_and_restore; then
  exit 1
fi

if git diff --exit-code . > /dev/null 2>&1 && [ -z "$(git ls-files --others --exclude-standard .)" ]; then
  success "Watcher recovered after the new dependency config was fixed"
else
  error "Dependency recovery test left repository changes"
  git diff .
  git ls-files --others --exclude-standard .
  exit 1
fi
