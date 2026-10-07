#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: Watcher keeps watching a source dir that was replaced at the same path"

error_output=$(rewatch clean 2>&1)
if [ $? -eq 0 ];
then
  success "Repo Cleaned"
else
  error "Error Cleaning Repo"
  printf "%s\n" "$error_output" >&2
  exit 1
fi

rewatch_bg watch > rewatch.log 2>&1 &
success "Watcher Started"

if ! wait_for_file "./src/Test.mjs" 20 || ! wait_for_pattern_count rewatch.log "Finished .*compilation" 1 30; then
  error "Initial build did not complete"
  cat rewatch.log
  exit_watcher
  exit 1
fi
success "Initial build completed"

cleanup() {
  rm -rf ./src-replaced
  git restore --worktree -- ./src/Test.res
}

# Replace the source dir with a copy. Some backends (inotify) tie a watch to the
# directory rather than to its path, so the new directory is not watched yet.
mv ./src ./src-replaced
cp -Rp ./src-replaced ./src
rm -rf ./src-replaced

# Trigger a full rebuild, which has to register a watch for the new directory.
# Any build counted below starts after the replacement, so it sees the new one.
completed_builds=$(grep -c "Finished .*compilation" rewatch.log 2>/dev/null || true)
touch rescript.json
if ! wait_for_pattern_count rewatch.log "Finished .*compilation" $((completed_builds + 1)) 30; then
  error "Full rebuild after touching rescript.json did not complete"
  cat rewatch.log
  cleanup
  exit_watcher
  exit 1
fi

completed_builds=$(grep -c "Finished .*compilation" rewatch.log 2>/dev/null || true)
echo 'Console.log("replaced-dir-probe")' >> ./src/Test.res

if wait_for_pattern_count ./src/Test.mjs "replaced-dir-probe" 1 20 \
  && wait_for_pattern_count rewatch.log "Finished .*compilation" $((completed_builds + 1)) 30; then
  success "Change in replaced source dir was compiled"
else
  error "Change in replaced source dir was not compiled"
  cat rewatch.log
  cleanup
  exit_watcher
  exit 1
fi

# Wait for the restored source to be compiled before stopping the watcher.
cleanup
timeout=$(platform_timeout 20)
while grep -q "replaced-dir-probe" ./src/Test.mjs && [ "$timeout" -gt 0 ]; do
  sleep 1
  timeout=$((timeout - 1))
done

if ! exit_watcher; then
  exit 1
fi

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
