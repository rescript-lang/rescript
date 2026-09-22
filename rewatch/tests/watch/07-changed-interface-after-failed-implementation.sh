#!/bin/bash

cd "$(dirname "$0")"
source "../utils.sh"

bold "Test: A changed interface dirties dependents after its implementation fails"

fixture=$(mktemp -d 2>/dev/null || mktemp -d -t rewatch-failed-implementation)
cleanup() {
  cd "$fixture" || return
  exit_watcher
  if [ -n "${watch_pid:-}" ]; then
    if process_is_running "$watch_pid"; then
      kill "$watch_pid" 2>/dev/null || true
    fi
    wait "$watch_pid" 2>/dev/null || true
  fi
  rm -rf "$fixture"
}
trap cleanup EXIT

mkdir -p "$fixture/src"
cat > "$fixture/rescript.json" <<'EOF'
{
  "name": "rewatch-failed-implementation",
  "sources": { "dir": "src" },
  "package-specs": { "module": "esmodule", "in-source": true },
  "suffix": ".mjs"
}
EOF

cat > "$fixture/src/Provider.resi" <<'EOF'
let value: int
EOF
cat > "$fixture/src/Provider.res" <<'EOF'
let value = 1
EOF
cat > "$fixture/src/Consumer.res" <<'EOF'
let number: int = Provider.value
EOF

cd "$fixture" || exit 1
RAYON_NUM_THREADS=1 "$REWATCH_EXECUTABLE" watch > rewatch.log 2>&1 &
watch_pid=$!
compiler_log=lib/bs/.compiler.log

wait_for_changed_compiler_log() {
  local baseline="$1"; local timeout
  timeout=$(platform_timeout 30)
  while [ "$timeout" -gt 0 ]; do
    if [ -f "$compiler_log" ] && ! cmp -s "$compiler_log" "$baseline" &&
      grep -q '#Done(' "$compiler_log"; then
      return 0
    fi
    sleep 1
    timeout=$((timeout - 1))
  done
  return 1
}

if ! wait_for_pattern_count rewatch.log "Finished .*compilation" 1 30; then
  error "Initial watch build did not finish"
  cat rewatch.log >&2
  exit 1
fi
cp "$compiler_log" initial.compiler.log

# The new interface writes a different CMI, but the old implementation cannot
# satisfy it. The dependent must remain blocked and dirty across this build.
printf 'let value: string\n' > src/Provider.resi
if ! wait_for_changed_compiler_log initial.compiler.log; then
  error "Watch did not finish the failing interface-change build"
  cat rewatch.log >&2
  exit 1
fi
if ! grep -q "Provider.res" "$compiler_log"; then
  error "Changed interface did not expose the implementation error"
  cat rewatch.log >&2
  exit 1
fi
cp "$compiler_log" failed.compiler.log

# Once the implementation satisfies the interface, Consumer must compile
# against the new string type and report its now-invalid int annotation.
printf 'let value = "recovered"\n' > src/Provider.res
if ! wait_for_changed_compiler_log failed.compiler.log; then
  error "Watch did not finish the recovery build"
  cat rewatch.log >&2
  exit 1
fi
if ! grep -q "Consumer.res" "$compiler_log"; then
  error "Consumer was not recompiled against the changed interface"
  cat rewatch.log >&2
  exit 1
fi

success "Changed CMI reaches blocked dependents after recovery"
