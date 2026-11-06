#!/bin/bash
set -eu
. "$(dirname "$0")/test_helpers.sh"

root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
rescript=${1:-$root/_build/default/rewatch-ocaml/rescript_ocaml.exe}
rescript=$(realpath "$rescript")
work=$(mktemp -d "${TMPDIR:-/tmp}/rewatch-interactive-output-XXXXXX")

if $windows_posix_shell; then
  echo "Skipping interactive output gate: a POSIX pseudo-terminal does not exercise the native Windows console"
  exit 0
fi

command_path() {
  if $windows_posix_shell; then
    cygpath -am "$1"
  else
    printf '%s\n' "$1"
  fi
}
active_script_pid=""
active_watch_project=""
active_watch_transcript=""
cleanup() {
  if [ -n "$active_script_pid" ]; then
    if [ -n "$active_watch_project" ]; then
      rm -f "$active_watch_project/lib/rescript/watch.lock"
    fi
    if ! wait_for_pid_gone "$active_script_pid" 50; then
      kill -TERM "$active_script_pid" 2>/dev/null || true
    fi
    if ! wait_for_pid_gone "$active_script_pid" 50; then
      kill -KILL "$active_script_pid" 2>/dev/null || true
    fi
    wait "$active_script_pid" 2>/dev/null || true
  fi
  rm -rf "$work"
}
trap cleanup EXIT

if ! command -v script >/dev/null 2>&1; then
  echo "Interactive output gate requires the util-linux script command" >&2
  exit 1
fi

# macOS buffers typescript files for up to 30 seconds unless -F is used. The
# watcher checks below must observe each message as soon as it is written.

normalize_output() {
  sed -E $'s/\033\\[[0-9;]*[[:alpha:]]//g' \
    | sed -e 's/\[clean\]/🧹/g' -e 's/\[parse\]/🧱/g' \
        -e 's/\[build\]/🤺/g' -e 's/\[ok\]/✅/g' \
        -e 's/\[warn\]/⚠️/g' -e 's/\[error\]/❌/g'
}

mkdir -p "$work/project/src"
printf '{"name":"interactive-output","sources":["src"],"namespace":"Interactive"}\n' \
  >"$work/project/rescript.json"
printf 'let value = 1\n' >"$work/project/src/A.res"
printf 'let value: int\n' >"$work/project/src/A.resi"
cp -R "$work/project" "$work/after-build"
cp -R "$work/project" "$work/watch"
cp -R "$work/project" "$work/initial-failure-watch"
printf 'let value = (\n' >"$work/initial-failure-watch/src/A.res"
cp -R "$root/rewatch-ocaml/tests/basic" "$work/partial-initial-failure-watch"
printf 'let answer: int = "not an int"\n' \
  >"$work/partial-initial-failure-watch/src/B.res"
cp -R "$work/project" "$work/warning-watch"
printf '%s\n' \
  '{"name":"interactive-output","sources":["src"],"namespace":"Interactive","package-specs":{"module":"es6","in-source":true}}' \
  >"$work/warning-watch/rescript.json"

cat >"$work/after-build-marker.sh" <<'EOF'
#!/bin/sh
printf '%s\n' AFTER_BUILD_MARKER
EOF
chmod +x "$work/after-build-marker.sh"
printf 'console.log("AFTER_BUILD_MARKER")\n' >"$work/after-build-marker.js"
cat >"$work/after-build-input.js" <<'EOF'
process.stdin.setEncoding("utf8");
process.stdin.once("data", input => {
  console.log(`AFTER_BUILD_INPUT:${input.trim()}`);
});
EOF

export RESCRIPT_RUNTIME=${RESCRIPT_RUNTIME:-$root/packages/@rescript/runtime}
after_build_command="$work/after-build-marker.sh"
if $windows_posix_shell; then
  RESCRIPT_RUNTIME=$(command_path "$RESCRIPT_RUNTIME")
  after_build_command="node $(command_path "$work/after-build-marker.js")"
fi

# run_in_terminal TRANSCRIPT ARGS...: runs rescript with ARGS under a
# pseudo-terminal, recording the terminal output in TRANSCRIPT.
run_in_terminal() {
  local transcript=$1
  shift
  if [ "$(uname -s)" = Darwin ]; then
    script -qF "$transcript" env -u NO_COLOR \
      "TERM=xterm" \
      "CLICOLOR=1" \
      "CLICOLOR_FORCE=0" \
      "RESCRIPT_RUNTIME=$RESCRIPT_RUNTIME" \
      "$rescript" "$@" >/dev/null
  else
    local command="env -u NO_COLOR TERM=xterm CLICOLOR=1 CLICOLOR_FORCE=0 RESCRIPT_RUNTIME=$RESCRIPT_RUNTIME $(command_path "$rescript")"
    local argument
    for argument in "$@"; do
      command="$command '$argument'"
    done
    script -qefc "$command" "$transcript" >/dev/null
  fi
}

echo "Checking interactive build output..."
run_in_terminal "$work/build.tty" build "$(command_path "$work/project")" --no-timing
tr '\r' '\n' <"$work/build.tty" \
  | normalize_output \
  | grep -E '^\[[123]/3\] .* (Cleaned|Parsed|Compiled) |^✅ Finished compilation' \
  >"$work/build.phases"

if ! grep -F $'\033[1m\033[2m[2/3]\033[0m' "$work/build.tty" >/dev/null; then
  echo "The build did not render the interactive step style" >&2
  cat "$work/build.tty" >&2
  exit 1
fi
tr '\r' '\n' <"$work/build.tty" | normalize_output >"$work/build.frames"
if ! grep -E '^\[2/3\] 🧱 Parsing\.\.\. .+ [0-9]+/1' \
    "$work/build.frames" >/dev/null || \
  ! grep -E '^\[3/3\] 🤺 Compiling\.\.\. .+ [0-9]+/2' \
    "$work/build.frames" >/dev/null; then
  echo "The build did not render both live spinner phases" >&2
  cat "$work/build.frames" >&2
  exit 1
fi

cat >"$work/expected" <<'EOF'
[1/3] 🧹 Cleaned 0/0
[2/3] 🧱 Parsed 1 source files
[3/3] 🤺 Compiled 1 modules
✅ Finished compilation
EOF

if ! cmp -s "$work/expected" "$work/build.phases"; then
  echo "Interactive phase output no longer has the expected stable shape" >&2
  cat "$work/build.phases" >&2
  exit 1
fi

echo "Checking interactive after-build output..."
run_in_terminal "$work/after-build.tty" \
  build --after-build "$after_build_command" \
  "$(command_path "$work/after-build")" --no-timing
tr '\r' '\n' <"$work/after-build.tty" \
  | normalize_output \
  | grep -E '^(✅ Finished compilation|AFTER_BUILD_MARKER)$' \
  >"$work/after-build.order"

cat >"$work/expected-after-build-order" <<'EOF'
✅ Finished compilation
AFTER_BUILD_MARKER
EOF

if ! cmp -s "$work/expected-after-build-order" "$work/after-build.order"; then
  echo "One-shot --after-build ran in the wrong phase" >&2
  cat "$work/after-build.order" >&2
  exit 1
fi

if $windows_posix_shell; then
  input_script=$(command_path "$work/after-build-input.js")
  transcript="$work/after-build-input.tty"
  printf 'from-terminal\n' | script -qefc \
    "env -u NO_COLOR TERM=xterm CLICOLOR=1 CLICOLOR_FORCE=0 RESCRIPT_RUNTIME=$RESCRIPT_RUNTIME $(command_path "$rescript") build --after-build 'node $input_script' $(command_path "$work/after-build") --no-timing" \
    "$transcript" >/dev/null
  if ! tr '\r' '\n' <"$transcript" \
    | grep -F 'AFTER_BUILD_INPUT:from-terminal' >/dev/null; then
    echo "Terminal input did not reach --after-build" >&2
    cat "$transcript" >&2
    exit 1
  fi
fi

echo "Checking quiet interactive builds..."
run_in_terminal "$work/quiet.tty" -q build "$(command_path "$work/project")"
if tr '\r' '\n' <"$work/quiet.tty" \
  | grep -E '(Cleaned|Parsed|Parsing\.\.\.|Compiled|Compiling\.\.\.|Finished .*compilation)' >/dev/null; then
  echo "A quiet interactive build emitted progress" >&2
  cat "$work/quiet.tty" >&2
  exit 1
fi

echo "Checking interactive clean output..."
run_in_terminal "$work/clean.tty" clean "$(command_path "$work/project")"
# The initial generic label is overwritten by the first package label on the
# same terminal line, so it is not part of the visible phase sequence.
tr '\r' '\n' <"$work/clean.tty" \
  | normalize_output \
  | grep -E '^\[[12]/2\] 🧹 (Cleaning|Cleaned)' \
  | awk '$0 != "[1/2] 🧹 Cleaning compiler assets..."' \
  | sed -E 's/in [0-9]+\.[0-9]+s$/in 0.00s/' \
  >"$work/clean.phases"

cat >"$work/expected-clean" <<'EOF'
[1/2] 🧹 Cleaning interactive-output...
[1/2] 🧹 Cleaned compiler assets in 0.00s
[2/2] 🧹 Cleaning .js files...
[2/2] 🧹 Cleaned .js files in 0.00s
EOF

if ! cmp -s "$work/expected-clean" "$work/clean.phases"; then
  echo "Interactive clean output no longer has the expected stable shape" >&2
  cat "$work/clean.phases" >&2
  exit 1
fi

echo "Checking quiet interactive cleans..."
run_in_terminal "$work/clean-quiet.tty" -q clean "$(command_path "$work/project")"
if tr '\r' '\n' <"$work/clean-quiet.tty" | grep -E '(Cleaning|Cleaned)' >/dev/null; then
  echo "A quiet interactive clean emitted progress" >&2
  cat "$work/clean-quiet.tty" >&2
  exit 1
fi

# Opening an existing source with shell redirection truncates it first. Some
# native watcher backends report that as a structural change, which correctly
# requests a full rebuild. Equal-length writes without truncation make these
# fixtures exercise the content-change/incremental path they are testing.
overwrite_line_in_place() {
  path=$1
  line=$2
  current_size=$(wc -c <"$path" | tr -d ' ')
  replacement_size=$(printf '%s\n' "$line" | wc -c | tr -d ' ')
  if [ "$current_size" -ne "$replacement_size" ]; then
    echo "In-place test update changed size for $path" >&2
    return 1
  fi
  printf '%s\n' "$line" | dd of="$path" conv=notrunc 2>/dev/null
}

stop_active_watch() {
  rm -f "$active_watch_project/lib/rescript/watch.lock"
  if ! wait_for_pid_gone "$active_script_pid"; then
    echo "Timed out waiting for the interactive watcher to stop" >&2
    if [ -f "$active_watch_transcript" ]; then
      cat "$active_watch_transcript" >&2
    fi
    return 1
  fi
  status=0
  wait "$active_script_pid" || status=$?
  active_script_pid=""
  active_watch_project=""
  active_watch_transcript=""
  return "$status"
}

# start_watch PROJECT TRANSCRIPT [ARGS...]: starts `rescript watch ARGS
# PROJECT` under a pseudo-terminal in the background.
start_watch() {
  local project=$1
  local transcript=$2
  shift 2
  run_in_terminal "$transcript" watch "$@" "$(command_path "$project")" &
  active_script_pid=$!
  active_watch_project=$project
  active_watch_transcript=$transcript
}

capture_watch_rebuild() {
  local project="$work/watch"
  local transcript="$work/watch.tty"
  start_watch "$project" "$transcript" --clear-screen
  if ! wait_for_text "$transcript" "Finished initial compilation" 1; then
    return 1
  fi
  overwrite_line_in_place "$project/src/A.res" 'let value = 2'
  if ! wait_for_text "$transcript" "Finished incremental compilation" 1; then
    return 1
  fi
  cp "$transcript" "$work/watch-phases.tty"
  overwrite_line_in_place "$project/src/A.res" 'let value = ('
  if ! wait_for_text "$transcript" "Build failed. Watching for changes..." 1; then
    return 1
  fi
  overwrite_line_in_place "$project/src/A.res" 'let value = 3'
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
  stop_active_watch
  tr '\r' '\n' <"$work/watch-phases.tty" \
    | normalize_output \
    | sed -E 's/in [0-9]+\.[0-9]+s/in <TIME>/' \
    >"$work/watch.normalized"
  awk '
    /^\[[123]\/3\] .* (Cleaned|Parsed|Compiled) / ||
      /^✅ Finished initial compilation in / {
      print
    }
    /^✅ Finished initial compilation in / { exit }
  ' "$work/watch.normalized" \
    >"$work/watch-initial.phases"
  grep -E '^\[[12]/2\] .* (Parsed|Compiled) |^✅ Finished incremental compilation in ' \
    "$work/watch.normalized" \
    >"$work/watch.phases"
  tr '\r' '\n' <"$transcript" \
    | normalize_output \
    >"$work/watch.presentation"
  rebuilds=$(grep -cF 'Change detected. Rebuilding...' \
    "$work/watch.presentation" || true)
  rebuilds=${rebuilds:-0}
  full_rebuilds=$(grep -cF 'Change detected. Full rebuild...' \
    "$work/watch.presentation" || true)
  full_rebuilds=${full_rebuilds:-0}
  failures=$(grep -cF 'Build failed. Watching for changes...' \
    "$work/watch.presentation" || true)
  failures=${failures:-0}
  if [ "$rebuilds" -lt 3 ] || [ "$full_rebuilds" -ne 1 ] || \
    [ "$failures" -ne 1 ]; then
    echo "The watch rebuild presentation changed" >&2
    cat "$work/watch.presentation" >&2
    exit 1
  fi
}

echo "Checking interactive watch rebuilds..."
capture_watch_rebuild

cat >"$work/expected-watch-initial" <<'EOF'
[1/3] 🧹 Cleaned 0/0 in <TIME>
[2/3] 🧱 Parsed 1 source files in <TIME>
[3/3] 🤺 Compiled 1 modules in <TIME>
✅ Finished initial compilation in <TIME>
EOF

if ! cmp -s "$work/expected-watch-initial" "$work/watch-initial.phases"; then
  echo "Interactive initial-watch output lost its stable shape" >&2
  cat "$work/watch-initial.phases" >&2
  exit 1
fi

cat >"$work/expected-watch" <<'EOF'
[1/2] 🧱 Parsed 1 source files in <TIME>
[2/2] 🤺 Compiled 1 modules in <TIME>
✅ Finished incremental compilation in <TIME>
EOF

if ! cmp -s "$work/expected-watch" "$work/watch.phases"; then
  echo "Interactive watch rebuild output lost its stable shape" >&2
  cat "$work/watch.phases" >&2
  exit 1
fi

capture_initial_failure_recovery() {
  local project="$work/initial-failure-watch"
  local transcript="$work/initial-failure-watch.tty"
  start_watch "$project" "$transcript" --clear-screen
  if ! wait_for_text "$transcript" "Error parsing source files" 1; then
    return 1
  fi
  overwrite_line_in_place "$project/src/A.res" 'let value = 1'
  if ! wait_for_text "$transcript" "Finished incremental compilation" 1; then
    return 1
  fi
  stop_active_watch
  tr '\r' '\n' <"$transcript" \
    | normalize_output \
    | sed -E 's/in [0-9]+\.[0-9]+s/in <TIME>/' \
    | sed -n '/Change detected\. Rebuilding\.\.\./,$p' \
    | grep -E '^(Change detected|\[[12]/2\] .* (Parsed|Compiled) |✅ Finished incremental compilation)' \
    | awk '{ print } /✅ Finished incremental compilation/ { exit }' \
    >"$work/initial-failure-recovery.phases"
}

echo "Checking interactive initial-failure recovery..."
capture_initial_failure_recovery

cat >"$work/expected-initial-failure-recovery" <<'EOF'
Change detected. Rebuilding...
[1/2] 🧱 Parsed 1 source files in <TIME>
[2/2] 🤺 Compiled 1 modules in <TIME>
✅ Finished incremental compilation in <TIME>
EOF

if ! cmp -s "$work/expected-initial-failure-recovery" \
  "$work/initial-failure-recovery.phases"; then
  echo "Initial-failure recovery did not retain incremental state" >&2
  cat "$work/initial-failure-recovery.phases" >&2
  exit 1
fi

capture_partial_initial_failure_recovery() {
  local project="$work/partial-initial-failure-watch"
  local transcript="$work/partial-initial-failure-watch.log"
  "$(command_path "$rescript")" watch "$(command_path "$project")" \
    >"$transcript" 2>&1 &
  active_script_pid=$!
  active_watch_project=$project
  active_watch_transcript=$transcript
  if ! wait_for_text "$transcript" "expected to have type" 1; then return 1; fi
  printf 'let answer = A.value + 1\n' >"$project/src/B.res"
  if ! wait_for_text "$transcript" "Finished incremental compilation" 1; then
    return 1
  fi
  wait_for_file "$project/src/A.mjs"
  wait_for_file "$project/src/B.mjs"
  if grep -F "I/O error: src/A.ast" "$transcript" >/dev/null; then
    echo "The watcher retained build state without its parsed AST" >&2
    cat "$transcript" >&2
    return 1
  fi
  stop_active_watch
}

echo "Checking partial initial-failure recovery..."
capture_partial_initial_failure_recovery

capture_warning_watch() {
  local project="$work/warning-watch"
  local transcript="$work/warning-watch.tty"
  start_watch "$project" "$transcript"
  if ! wait_for_text "$transcript" "Finished initial compilation" 1; then
    return 1
  fi
  overwrite_line_in_place "$project/src/A.res" 'let value = 2'
  if ! wait_for_text "$transcript" "Finished incremental compilation" 1; then
    return 1
  fi
  printf 'let added = 1\n' >"$project/src/B.res"
  if ! wait_for_text "$transcript" "Finished compilation" 1; then
    return 1
  fi
  stop_active_watch

  if [ "$(grep -cF "uses deprecated config" "$transcript")" -ne 1 ]; then
    echo "The watcher repeated a configuration warning" >&2
    cat "$transcript" >&2
    exit 1
  fi
  if grep -F "Finished incremental compilation with warnings" \
    "$transcript" >/dev/null; then
    echo "A static configuration warning was carried into the incremental footer" >&2
    cat "$transcript" >&2
    exit 1
  fi
  if ! grep -F $'\033[33mPackage' "$transcript" >/dev/null; then
    echo "Interactive configuration warnings were not rendered in yellow" >&2
    cat "$transcript" >&2
    exit 1
  fi
}

echo "Checking interactive watch warnings..."
capture_warning_watch

echo "Interactive output phases matched"
