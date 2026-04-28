#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: Warnings persist after atomic-save rename of unrelated module"

error_output=$(rewatch clean 2>&1)
if [ $? -eq 0 ];
then
  success "Repo Cleaned"
else
  error "Error Cleaning Repo"
  printf "%s\n" "$error_output" >&2
  exit 1
fi

wait_for_pattern() {
  local file="$1"; local pattern="$2"; local timeout="${3:-30}"
  while [ "$timeout" -gt 0 ]; do
    grep -q "$pattern" "$file" 2>/dev/null && return 0
    sleep 1
    timeout=$((timeout - 1))
  done
  return 1
}

wait_for_changed_completed_log() {
  local file="$1"; local baseline="$2"; local timeout="${3:-30}"
  while [ "$timeout" -gt 0 ]; do
    if [ -f "$file" ] && ! cmp -s "$file" "$baseline" && grep -q "#Done(" "$file" 2>/dev/null; then
      return 0
    fi
    sleep 1
    timeout=$((timeout - 1))
  done
  return 1
}

COMPILER_LOG="./packages/watch-warnings/lib/bs/.compiler.log"
B_FILE="./packages/watch-warnings/src/B.res"
WATCH_STDOUT="rewatch-stdout.log"
WATCH_STDERR="rewatch-stderr.log"
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/rewatch-watch-warning-atomic-save.XXXXXX")"
INITIAL_LOG="$TMP_DIR/initial.compiler.log"
BACKUP_B="$TMP_DIR/B.res.bak"

cp "$B_FILE" "$BACKUP_B"

cleanup() {
  set +e
  if [ -f "$BACKUP_B" ]; then
    cp "$BACKUP_B" "$B_FILE"
  fi
  exit_watcher
  sleep 1
  rm -rf "$TMP_DIR"
  rm -f "$WATCH_STDOUT" "$WATCH_STDERR"
}
trap cleanup EXIT

rewatch_bg watch > "$WATCH_STDOUT" 2> "$WATCH_STDERR" &

if ! wait_for_pattern "$COMPILER_LOG" "unused value unusedValue" 30 || ! wait_for_pattern "$COMPILER_LOG" "#Done(" 30; then
  error "Initial build did not finish with warning from ModuleA.res in $COMPILER_LOG"
  echo
  echo "=== compiler log ==="
  cat "$COMPILER_LOG" 2>/dev/null || true
  echo
  echo "=== watch stderr ==="
  cat "$WATCH_STDERR" 2>/dev/null || true
  echo
  echo "=== watch stdout ==="
  cat "$WATCH_STDOUT" 2>/dev/null || true
  exit 1
fi
success "Initial build shows warning from ModuleA.res in $COMPILER_LOG"

cp "$COMPILER_LOG" "$INITIAL_LOG"

# Simulate an editor atomic save by writing a temp file and renaming it into place.
tmp_b_save="$(mktemp "${TMPDIR:-/tmp}/B.res.XXXXXX")"
printf 'let world = () => Console.log("world")\n// trigger atomic save\n' > "$tmp_b_save"
mv "$tmp_b_save" "$B_FILE"

if ! wait_for_changed_completed_log "$COMPILER_LOG" "$INITIAL_LOG" 30; then
  error "Compiler log did not complete a new cycle after atomic save of B.res"
  echo
  echo "=== compiler log ==="
  cat "$COMPILER_LOG" 2>/dev/null || true
  echo
  echo "=== watch stderr ==="
  cat "$WATCH_STDERR" 2>/dev/null || true
  echo
  echo "=== watch stdout ==="
  cat "$WATCH_STDOUT" 2>/dev/null || true
  exit 1
fi

if grep -q "unused value unusedValue" "$COMPILER_LOG"; then
  success "Warning from ModuleA.res persists after atomic save of B.res"
else
  error "Warning from ModuleA.res was lost after atomic save of B.res"
  echo
  echo "=== compiler log ==="
  cat "$COMPILER_LOG"
  echo
  echo "=== watch stderr ==="
  cat "$WATCH_STDERR" 2>/dev/null || true
  echo
  echo "=== watch stdout ==="
  cat "$WATCH_STDOUT" 2>/dev/null || true
  exit 1
fi

cp "$BACKUP_B" "$B_FILE"
sleep 1
exit_watcher
sleep 2
rm -f "$WATCH_STDOUT" "$WATCH_STDERR"

if git diff --exit-code ./packages/watch-warnings > /dev/null 2>&1;
then
  success "No leftover changes in watch-warnings package"
else
  error "Leftover changes detected in watch-warnings package"
  git diff ./packages/watch-warnings
  exit 1
fi
