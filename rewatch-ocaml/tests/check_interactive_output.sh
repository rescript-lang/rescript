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
  printf '{"name":"interactive-output","sources":["src"]}\n' \
    >"$work/$implementation/rescript.json"
  printf 'let value = 1\n' >"$work/$implementation/src/A.res"
  cp -R "$work/$implementation" "$work/$implementation-watch"
done

export RESCRIPT_BSC_EXE=${RESCRIPT_BSC_EXE:-$root/_build/default/compiler/bsc/rescript_compiler_main.exe}
export RESCRIPT_RUNTIME=${RESCRIPT_RUNTIME:-$root/packages/@rescript/runtime}

capture() {
  implementation=$1
  executable=$2
  transcript="$work/$implementation.tty"
  if [ "$(uname -s)" = Darwin ]; then
    script -q "$transcript" env \
      "RESCRIPT_BSC_EXE=$RESCRIPT_BSC_EXE" \
      "RESCRIPT_RUNTIME=$RESCRIPT_RUNTIME" \
      "$executable" build "$work/$implementation" --no-timing >/dev/null
  else
    script -qefc \
      "RESCRIPT_BSC_EXE=$RESCRIPT_BSC_EXE RESCRIPT_RUNTIME=$RESCRIPT_RUNTIME $executable build $work/$implementation --no-timing" \
      "$transcript" >/dev/null
  fi
  tr '\r' '\n' <"$transcript" \
    | sed -E $'s/\033\\[[0-9;]*[[:alpha:]]//g' \
    | grep -E '^\[[123]/3\] .* (Cleaned|Parsed|Compiled) |^✅ Finished compilation in ' \
    >"$work/$implementation.phases"
}

capture rust "$rust"
capture ocaml "$ocaml"

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
    script -q "$transcript" env \
      "RESCRIPT_BSC_EXE=$RESCRIPT_BSC_EXE" \
      "RESCRIPT_RUNTIME=$RESCRIPT_RUNTIME" \
      "$executable" -q build "$work/$implementation" >/dev/null
  else
    script -qefc \
      "RESCRIPT_BSC_EXE=$RESCRIPT_BSC_EXE RESCRIPT_RUNTIME=$RESCRIPT_RUNTIME $executable -q build $work/$implementation" \
      "$transcript" >/dev/null
  fi
  if tr '\r' '\n' <"$transcript" \
    | grep -E '(Cleaned|Parsed|Compiled|Finished .*compilation)' >/dev/null; then
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
  return 1
}

capture_watch_rebuild() {
  local implementation=$1
  local executable=$2
  local project="$work/$implementation-watch"
  local transcript="$work/$implementation-watch.tty"
  if [ "$(uname -s)" = Darwin ]; then
    script -q "$transcript" env \
      "RESCRIPT_BSC_EXE=$RESCRIPT_BSC_EXE" \
      "RESCRIPT_RUNTIME=$RESCRIPT_RUNTIME" \
      "$executable" watch "$project" >/dev/null &
  else
    script -qefc \
      "RESCRIPT_BSC_EXE=$RESCRIPT_BSC_EXE RESCRIPT_RUNTIME=$RESCRIPT_RUNTIME $executable watch $project" \
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
  tr '\r' '\n' <"$transcript" \
    | sed -E $'s/\033\\[[0-9;]*[[:alpha:]]//g; s/in [0-9]+\\.[0-9]+s/in <TIME>/' \
    >"$work/$implementation-watch.normalized"
  grep -E '^\[[123]/3\] .* (Cleaned|Parsed|Compiled) |^✅ Finished initial compilation in ' \
    "$work/$implementation-watch.normalized" \
    >"$work/$implementation-watch-initial.phases"
  grep -E '^\[[12]/2\] .* (Parsed|Compiled) |^✅ Finished incremental compilation in ' \
    "$work/$implementation-watch.normalized" \
    >"$work/$implementation-watch.phases"
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

echo "Interactive output phases matched"
