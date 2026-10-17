#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: Watcher keeps watching a folder when an overlapping source folder is removed"

error_output=$(rewatch clean 2>&1)
if [ $? -eq 0 ];
then
  success "Repo Cleaned"
else
  error "Error Cleaning Repo"
  printf "%s\n" "$error_output" >&2
  exit 1
fi

DEP01_CONFIG=packages/dep01/rescript.json
RETAINED=packages/dep01/src/retained

cleanup() {
  git restore --worktree -- "$DEP01_CONFIG"
  rm -rf "$RETAINED"
  rm -f rewatch.log
}

# src/retained is watched both through the recursive src folder and as a
# source folder of its own
mkdir -p "$RETAINED"
echo 'let value = 1' > "$RETAINED/Retained.res"
node -e "
  const fs = require('fs');
  const config = JSON.parse(fs.readFileSync('$DEP01_CONFIG', 'utf8'));
  config.sources = [{dir: 'src', subdirs: true}, 'src/retained'];
  fs.writeFileSync('$DEP01_CONFIG', JSON.stringify(config, null, 2) + '\n');
"

retained_output() {
  find "$RETAINED" -name 'Retained.*js' | head -n 1
}

rewatch_bg watch > rewatch.log 2>&1 &
success "Watcher Started"

wait_for_retained_value() {
  local value=$1
  local timeout=30
  while [ "$timeout" -gt 0 ]; do
    output=$(retained_output)
    if [ -n "$output" ] && grep -q "$value" "$output"; then
      return 0
    fi
    sleep 1
    timeout=$((timeout - 1))
  done
  return 1
}

if ! wait_for_retained_value "= 1"; then
  error "Initial build did not compile the retained folder"
  cat rewatch.log
  exit_watcher
  cleanup
  exit 1
fi
success "Initial build completed"

# A folder created inside src/retained belongs to the recursive src folder, so
# the watch of src/retained must stay recursive although src/retained is also
# watched on its own
mkdir "$RETAINED/nested"
echo 'let value = 1' > "$RETAINED/nested/Nested.res"
timeout=30
until [ -n "$(find "$RETAINED/nested" -name 'Nested.*js')" ] || [ "$timeout" -le 0 ]; do
  sleep 1
  timeout=$((timeout - 1))
done
if [ -n "$(find "$RETAINED/nested" -name 'Nested.*js')" ]; then
  success "A folder created inside the retained folder is watched"
else
  error "A folder created inside the retained folder is not watched"
  cat rewatch.log
  exit_watcher
  cleanup
  exit 1
fi

completed_builds=$(grep -c "Finished .*compilation" rewatch.log 2>/dev/null || true)

# Remove the recursive src folder while keeping src itself (non-recursive)
# and src/retained
node -e "
  const fs = require('fs');
  const config = JSON.parse(fs.readFileSync('$DEP01_CONFIG', 'utf8'));
  config.sources = ['src', 'src/retained'];
  fs.writeFileSync('$DEP01_CONFIG', JSON.stringify(config, null, 2) + '\n');
"
if ! wait_for_pattern_count rewatch.log "Finished .*compilation" \
  $((completed_builds + 1)) 30; then
  error "No rebuild after the configuration change"
  cat rewatch.log
  exit_watcher
  cleanup
  exit 1
fi

echo 'let value = 2' > "$RETAINED/Retained.res"
if wait_for_retained_value "= 2"; then
  success "Edit in the retained folder was compiled"
else
  error "Edit in the retained folder was not picked up"
  cat rewatch.log
  exit_watcher
  cleanup
  exit 1
fi

if ! exit_watcher; then
  cleanup
  exit 1
fi
cleanup

# Rebuild to regenerate the outputs removed by `rewatch clean`
rewatch build > /dev/null 2>&1

if git diff --exit-code . > /dev/null 2>&1 && [ -z "$(git ls-files --others --exclude-standard .)" ];
then
  success "No leftover changes"
else
  error "Leftover changes detected"
  git diff .
  git ls-files --others --exclude-standard .
  exit 1
fi
