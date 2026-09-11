#!/bin/bash
set -eu

root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
rust=${1:-$root/rewatch/target/debug/rescript}
ocaml=${2:-$root/_build/default/rewatch-ocaml/rescript_ocaml.exe}
rust=$(realpath "$rust")
ocaml=$(realpath "$ocaml")
work=$(mktemp -d "${TMPDIR:-/tmp}/rewatch-interactive-output-XXXXXX")
active_script_pid=""
cleanup() {
  if [ -n "$active_script_pid" ]; then
    kill -TERM "$active_script_pid" 2>/dev/null || true
    wait "$active_script_pid" 2>/dev/null || true
  fi
  rm -rf "$work"
}
trap cleanup EXIT

if ! command -v script >/dev/null 2>&1; then
  echo "Interactive output gate requires the util-linux script command" >&2
  exit 1
fi

for implementation in rust ocaml; do
  mkdir -p "$work/$implementation/src"
  printf '{"name":"interactive-output","sources":["src"],"namespace":"Interactive"}\n' \
    >"$work/$implementation/rescript.json"
  printf 'let value = 1\n' >"$work/$implementation/src/A.res"
  printf 'let value: int\n' >"$work/$implementation/src/A.resi"
  cp -R "$work/$implementation" "$work/$implementation-watch"
  cp -R "$work/$implementation" "$work/$implementation-initial-failure-watch"
  printf 'let value =\n' \
    >"$work/$implementation-initial-failure-watch/src/A.res"
  cp -R "$root/rewatch-ocaml/tests/basic" \
    "$work/$implementation-partial-initial-failure-watch"
  printf 'let answer: int = "not an int"\n' \
    >"$work/$implementation-partial-initial-failure-watch/src/B.res"
  cp -R "$work/$implementation" "$work/$implementation-warning-watch"
  printf '%s\n' \
    '{"name":"interactive-output","sources":["src"],"namespace":"Interactive","package-specs":{"module":"es6","in-source":true}}' \
    >"$work/$implementation-warning-watch/rescript.json"
done

export RESCRIPT_BSC_EXE=${RESCRIPT_BSC_EXE:-$root/_build/default/compiler/bsc/rescript_compiler_main.exe}
export RESCRIPT_RUNTIME=${RESCRIPT_RUNTIME:-$root/packages/@rescript/runtime}

capture() {
  implementation=$1
  executable=$2
  transcript="$work/$implementation.tty"
  if [ "$(uname -s)" = Darwin ]; then
    script -q "$transcript" env -u NO_COLOR \
      "TERM=xterm" \
      "CLICOLOR=1" \
      "CLICOLOR_FORCE=0" \
      "RESCRIPT_BSC_EXE=$RESCRIPT_BSC_EXE" \
      "RESCRIPT_RUNTIME=$RESCRIPT_RUNTIME" \
      "$executable" build "$work/$implementation" --no-timing >/dev/null
  else
    script -qefc \
      "env -u NO_COLOR TERM=xterm CLICOLOR=1 CLICOLOR_FORCE=0 RESCRIPT_BSC_EXE=$RESCRIPT_BSC_EXE RESCRIPT_RUNTIME=$RESCRIPT_RUNTIME $executable build $work/$implementation --no-timing" \
      "$transcript" >/dev/null
  fi
  tr '\r' '\n' <"$transcript" \
    | sed -E $'s/\033\\[[0-9;]*[[:alpha:]]//g' \
    | grep -E '^\[[123]/3\] .* (Cleaned|Parsed|Compiled) |^✅ Finished compilation in ' \
    >"$work/$implementation.phases"
}

capture rust "$rust"
capture ocaml "$ocaml"

require_spinner_frames() {
  implementation=$1
  if ! grep -F $'\033[1m\033[2m[2/3]\033[0m' \
      "$work/$implementation.tty" >/dev/null; then
    echo "$implementation did not render the interactive step style" >&2
    cat "$work/$implementation.tty" >&2
    exit 1
  fi
  tr '\r' '\n' <"$work/$implementation.tty" \
    | sed -E $'s/\033\\[[0-9;]*[[:alpha:]]//g' \
    >"$work/$implementation.frames"
  if ! grep -E '^\[2/3\] 🧱 Parsing\.\.\. .+ [0-9]+/1' \
      "$work/$implementation.frames" >/dev/null || \
    ! grep -E '^\[3/3\] 🤺 Compiling\.\.\. .+ [0-9]+/2' \
      "$work/$implementation.frames" >/dev/null; then
    echo "$implementation did not render both live spinner phases" >&2
    cat "$work/$implementation.frames" >&2
    exit 1
  fi
}

require_spinner_frames rust
require_spinner_frames ocaml

if ! cmp -s "$work/rust.phases" "$work/ocaml.phases"; then
  echo "Interactive phase output differs" >&2
  printf '%s\n' '--- Rust phases ---' >&2
  cat "$work/rust.phases" >&2
  printf '%s\n' '--- OCaml phases ---' >&2
  cat "$work/ocaml.phases" >&2
  exit 1
fi

cat >"$work/expected" <<'EOF'
[1/3] 🧹 Cleaned 0/0 in 0.00s
[2/3] 🧱 Parsed 1 source files in 0.00s
[3/3] 🤺 Compiled 1 modules in 0.00s
✅ Finished compilation in 0.00s
EOF

if ! cmp -s "$work/expected" "$work/ocaml.phases"; then
  echo "Interactive phase output no longer has the expected stable shape" >&2
  cat "$work/ocaml.phases" >&2
  exit 1
fi

capture_quiet_build() {
  implementation=$1
  executable=$2
  transcript="$work/$implementation-quiet.tty"
  if [ "$(uname -s)" = Darwin ]; then
    script -q "$transcript" env -u NO_COLOR \
      "TERM=xterm" \
      "CLICOLOR=1" \
      "CLICOLOR_FORCE=0" \
      "RESCRIPT_BSC_EXE=$RESCRIPT_BSC_EXE" \
      "RESCRIPT_RUNTIME=$RESCRIPT_RUNTIME" \
      "$executable" -q build "$work/$implementation" >/dev/null
  else
    script -qefc \
      "env -u NO_COLOR TERM=xterm CLICOLOR=1 CLICOLOR_FORCE=0 RESCRIPT_BSC_EXE=$RESCRIPT_BSC_EXE RESCRIPT_RUNTIME=$RESCRIPT_RUNTIME $executable -q build $work/$implementation" \
      "$transcript" >/dev/null
  fi
  if tr '\r' '\n' <"$transcript" \
    | grep -E '(Cleaned|Parsed|Parsing\.\.\.|Compiled|Compiling\.\.\.|Finished .*compilation)' >/dev/null; then
    echo "$implementation quiet interactive build emitted progress" >&2
    cat "$transcript" >&2
    exit 1
  fi
}

capture_quiet_build rust "$rust"
capture_quiet_build ocaml "$ocaml"

wait_for_text() {
  path=$1
  pattern=$2
  count=$3
  attempts=0
  while [ "$attempts" -lt 200 ]; do
    actual=$(grep -cF "$pattern" "$path" 2>/dev/null || true)
    if [ "$actual" -ge "$count" ]; then
      return 0
    fi
    attempts=$((attempts + 1))
    sleep 0.1
  done
  printf 'Timed out waiting for occurrence %s of %s in %s\n' \
    "$count" "$pattern" "$path" >&2
  if [ -f "$path" ]; then cat "$path" >&2; fi
  return 1
}

wait_for_file() {
  path=$1
  attempts=0
  while [ "$attempts" -lt 200 ]; do
    if [ -f "$path" ]; then return 0; fi
    attempts=$((attempts + 1))
    sleep 0.1
  done
  printf 'Timed out waiting for %s\n' "$path" >&2
  return 1
}

capture_watch_rebuild() {
  local implementation=$1
  local executable=$2
  local project="$work/$implementation-watch"
  local transcript="$work/$implementation-watch.tty"
  if [ "$(uname -s)" = Darwin ]; then
    script -q "$transcript" env -u NO_COLOR \
      "TERM=xterm" \
      "CLICOLOR=1" \
      "CLICOLOR_FORCE=0" \
      "RESCRIPT_BSC_EXE=$RESCRIPT_BSC_EXE" \
      "RESCRIPT_RUNTIME=$RESCRIPT_RUNTIME" \
      "$executable" watch --clear-screen "$project" >/dev/null &
  else
    script -qefc \
      "env -u NO_COLOR TERM=xterm CLICOLOR=1 CLICOLOR_FORCE=0 RESCRIPT_BSC_EXE=$RESCRIPT_BSC_EXE RESCRIPT_RUNTIME=$RESCRIPT_RUNTIME $executable watch --clear-screen $project" \
      "$transcript" >/dev/null &
  fi
  active_script_pid=$!
  if ! wait_for_text "$transcript" "Finished initial compilation" 1; then
    return 1
  fi
  printf 'let value = 2\n' >"$project/src/A.res"
  if ! wait_for_text "$transcript" "Finished incremental compilation" 1; then
    return 1
  fi
  cp "$transcript" "$work/$implementation-watch-phases.tty"
  printf 'let value =\n' >"$project/src/A.res"
  if ! wait_for_text "$transcript" "Build failed. Watching for changes..." 1; then
    return 1
  fi
  printf 'let value = 3\n' >"$project/src/A.res"
  if ! wait_for_text "$transcript" "Finished incremental compilation" 2; then
    return 1
  fi
  printf '%s\n' \
    '{"name":"interactive-output","sources":["src"],"namespace":"Interactive","package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
    >"$project/rescript.next"
  mv "$project/rescript.next" "$project/rescript.json"
  if ! wait_for_text "$transcript" "Change detected. Full rebuild..." 1 || \
    ! wait_for_text "$transcript" "Finished compilation" 1; then
    return 1
  fi
  rm -f "$project/lib/watch.lock"
  wait "$active_script_pid"
  active_script_pid=""
  tr '\r' '\n' <"$work/$implementation-watch-phases.tty" \
    | sed -E $'s/\033\\[[0-9;]*[[:alpha:]]//g; s/in [0-9]+\\.[0-9]+s/in <TIME>/' \
    >"$work/$implementation-watch.normalized"
  grep -E '^\[[123]/3\] .* (Cleaned|Parsed|Compiled) |^✅ Finished initial compilation in ' \
    "$work/$implementation-watch.normalized" \
    >"$work/$implementation-watch-initial.phases"
  grep -E '^\[[12]/2\] .* (Parsed|Compiled) |^✅ Finished incremental compilation in ' \
    "$work/$implementation-watch.normalized" \
    >"$work/$implementation-watch.phases"
  tr '\r' '\n' <"$transcript" \
    | sed -E $'s/\033\\[[0-9;]*[[:alpha:]]//g' \
    >"$work/$implementation-watch.presentation"
  if [ "$(grep -cF 'Change detected. Rebuilding...' \
      "$work/$implementation-watch.presentation")" -lt 3 ] || \
    [ "$(grep -cF 'Change detected. Full rebuild...' \
      "$work/$implementation-watch.presentation")" -ne 1 ] || \
    [ "$(grep -cF 'Build failed. Watching for changes...' \
      "$work/$implementation-watch.presentation")" -ne 1 ]; then
    echo "$implementation watch rebuild presentation changed" >&2
    cat "$work/$implementation-watch.presentation" >&2
    exit 1
  fi
}

capture_watch_rebuild rust "$rust"
capture_watch_rebuild ocaml "$ocaml"

if ! cmp -s "$work/rust-watch-initial.phases" \
  "$work/ocaml-watch-initial.phases"; then
  echo "Interactive initial-watch output differs" >&2
  printf '%s\n' '--- Rust initial phases ---' >&2
  cat "$work/rust-watch-initial.phases" >&2
  printf '%s\n' '--- OCaml initial phases ---' >&2
  cat "$work/ocaml-watch-initial.phases" >&2
  exit 1
fi

cat >"$work/expected-watch-initial" <<'EOF'
[1/3] 🧹 Cleaned 0/0 in <TIME>
[2/3] 🧱 Parsed 1 source files in <TIME>
[3/3] 🤺 Compiled 1 modules in <TIME>
✅ Finished initial compilation in <TIME>
EOF

if ! cmp -s "$work/expected-watch-initial" \
  "$work/ocaml-watch-initial.phases"; then
  echo "Interactive initial-watch output lost its stable shape" >&2
  cat "$work/ocaml-watch-initial.phases" >&2
  exit 1
fi

if ! cmp -s "$work/rust-watch.phases" "$work/ocaml-watch.phases"; then
  echo "Interactive watch rebuild output differs" >&2
  printf '%s\n' '--- Rust rebuild phases ---' >&2
  cat "$work/rust-watch.phases" >&2
  printf '%s\n' '--- OCaml rebuild phases ---' >&2
  cat "$work/ocaml-watch.phases" >&2
  exit 1
fi

cat >"$work/expected-watch" <<'EOF'
[1/2] 🧱 Parsed 1 source files in <TIME>
[2/2] 🤺 Compiled 1 modules in <TIME>
✅ Finished incremental compilation in <TIME>
EOF

if ! cmp -s "$work/expected-watch" "$work/ocaml-watch.phases"; then
  echo "Interactive watch rebuild output lost its stable shape" >&2
  cat "$work/ocaml-watch.phases" >&2
  exit 1
fi

capture_initial_failure_recovery() {
  local implementation=$1
  local executable=$2
  local project="$work/$implementation-initial-failure-watch"
  local transcript="$work/$implementation-initial-failure-watch.tty"
  if [ "$(uname -s)" = Darwin ]; then
    script -q "$transcript" env -u NO_COLOR \
      "TERM=xterm" \
      "CLICOLOR=1" \
      "CLICOLOR_FORCE=0" \
      "RESCRIPT_BSC_EXE=$RESCRIPT_BSC_EXE" \
      "RESCRIPT_RUNTIME=$RESCRIPT_RUNTIME" \
      "$executable" watch --clear-screen "$project" >/dev/null &
  else
    script -qefc \
      "env -u NO_COLOR TERM=xterm CLICOLOR=1 CLICOLOR_FORCE=0 RESCRIPT_BSC_EXE=$RESCRIPT_BSC_EXE RESCRIPT_RUNTIME=$RESCRIPT_RUNTIME $executable watch --clear-screen $project" \
      "$transcript" >/dev/null &
  fi
  active_script_pid=$!
  if ! wait_for_text "$transcript" "Error parsing source files" 1; then
    return 1
  fi
  printf 'let value = 1\n' >"$project/src/A.res"
  if ! wait_for_text "$transcript" "Finished incremental compilation" 1; then
    return 1
  fi
  rm -f "$project/lib/watch.lock"
  wait "$active_script_pid"
  active_script_pid=""
  tr '\r' '\n' <"$transcript" \
    | sed -E $'s/\033\\[[0-9;]*[[:alpha:]]//g; s/in [0-9]+\\.[0-9]+s/in <TIME>/' \
    | sed -n '/Change detected\. Rebuilding\.\.\./,$p' \
    | grep -E '^(Change detected|\[[12]/2\] .* (Parsed|Compiled) |✅ Finished incremental compilation)' \
    >"$work/$implementation-initial-failure-recovery.phases"
}

capture_initial_failure_recovery rust "$rust"
capture_initial_failure_recovery ocaml "$ocaml"

if ! cmp -s "$work/rust-initial-failure-recovery.phases" \
  "$work/ocaml-initial-failure-recovery.phases"; then
  echo "Interactive initial-failure recovery output differs" >&2
  printf '%s\n' '--- Rust recovery phases ---' >&2
  cat "$work/rust-initial-failure-recovery.phases" >&2
  printf '%s\n' '--- OCaml recovery phases ---' >&2
  cat "$work/ocaml-initial-failure-recovery.phases" >&2
  exit 1
fi

cat >"$work/expected-initial-failure-recovery" <<'EOF'
Change detected. Rebuilding...
[1/2] 🧱 Parsed 1 source files in <TIME>
[2/2] 🤺 Compiled 1 modules in <TIME>
✅ Finished incremental compilation in <TIME>
EOF

if ! cmp -s "$work/expected-initial-failure-recovery" \
  "$work/ocaml-initial-failure-recovery.phases"; then
  echo "Initial-failure recovery did not retain incremental state" >&2
  cat "$work/ocaml-initial-failure-recovery.phases" >&2
  exit 1
fi

capture_partial_initial_failure_recovery() {
  local implementation=$1
  local executable=$2
  local project="$work/$implementation-partial-initial-failure-watch"
  local transcript="$work/$implementation-partial-initial-failure-watch.log"
  "$executable" watch "$project" >"$transcript" 2>&1 &
  active_script_pid=$!
  if ! wait_for_text "$transcript" "expected to have type" 1; then return 1; fi
  printf 'let answer = A.value + 1\n' >"$project/src/B.res"
  if ! wait_for_text "$transcript" "Finished incremental compilation" 1; then
    return 1
  fi
  wait_for_file "$project/src/A.mjs"
  wait_for_file "$project/src/B.mjs"
  if grep -F "I/O error: src/A.ast" "$transcript" >/dev/null; then
    echo "$implementation retained build state without its parsed AST" >&2
    cat "$transcript" >&2
    return 1
  fi
  rm -f "$project/lib/watch.lock"
  wait "$active_script_pid"
  active_script_pid=""
}

capture_partial_initial_failure_recovery rust "$rust"
capture_partial_initial_failure_recovery ocaml "$ocaml"

capture_warning_watch() {
  local implementation=$1
  local executable=$2
  local project="$work/$implementation-warning-watch"
  local transcript="$work/$implementation-warning-watch.tty"
  if [ "$(uname -s)" = Darwin ]; then
    script -q "$transcript" env -u NO_COLOR \
      "TERM=xterm" \
      "CLICOLOR=1" \
      "CLICOLOR_FORCE=0" \
      "RESCRIPT_BSC_EXE=$RESCRIPT_BSC_EXE" \
      "RESCRIPT_RUNTIME=$RESCRIPT_RUNTIME" \
      "$executable" watch "$project" >/dev/null &
  else
    script -qefc \
      "env -u NO_COLOR TERM=xterm CLICOLOR=1 CLICOLOR_FORCE=0 RESCRIPT_BSC_EXE=$RESCRIPT_BSC_EXE RESCRIPT_RUNTIME=$RESCRIPT_RUNTIME $executable watch $project" \
      "$transcript" >/dev/null &
  fi
  active_script_pid=$!
  if ! wait_for_text "$transcript" "Finished initial compilation" 1; then
    return 1
  fi
  printf 'let value = 2\n' >"$project/src/A.res"
  if ! wait_for_text "$transcript" "Finished incremental compilation" 1; then
    return 1
  fi
  rm -f "$project/lib/watch.lock"
  wait "$active_script_pid"
  active_script_pid=""

  if [ "$(grep -cF "uses deprecated config" "$transcript")" -ne 1 ]; then
    echo "$implementation repeated a configuration warning during watch" >&2
    cat "$transcript" >&2
    exit 1
  fi
  if grep -F "Finished incremental compilation with warnings" \
    "$transcript" >/dev/null; then
    echo "$implementation carried a static configuration warning into the incremental footer" >&2
    cat "$transcript" >&2
    exit 1
  fi
  if [ "$implementation" = ocaml ] && \
    ! grep -F $'\033[33mPackage' "$transcript" >/dev/null; then
    echo "$implementation did not render interactive configuration warnings in yellow" >&2
    cat "$transcript" >&2
    exit 1
  fi
}

capture_warning_watch rust "$rust"
capture_warning_watch ocaml "$ocaml"

echo "Interactive output phases matched"
