#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "Usage: $0 RUST_REWATCH OCAML_REWATCH" >&2
  exit 2
fi

repo_root=$(cd "$(dirname "$0")/../.." && pwd)
rust_executable=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
ocaml_executable=$(cd "$(dirname "$2")" && pwd)/$(basename "$2")
normalizer="$repo_root/rewatch-ocaml/bench/normalize_file_trace.js"

for command in cp date find grep join mktemp node sed seq sleep sort strace wc; do
  command -v "$command" >/dev/null || {
    echo "Missing required command: $command" >&2
    exit 2
  }
done
if [[ ! -x "$rust_executable" || ! -x "$ocaml_executable" ]]; then
  echo "Both rewatch executables must exist and be executable." >&2
  exit 2
fi

if [[ -z ${RESCRIPT_BSC_EXE:-} || -z ${RESCRIPT_RUNTIME:-} ]]; then
  eval "$(cd "$repo_root/rewatch/tests" && node ./get_bin_paths.js)"
fi
export RESCRIPT_BSC_EXE RESCRIPT_RUNTIME

work_root=$(mktemp -d "${TMPDIR:-/tmp}/rewatch-watch-audit.XXXXXX")
background_pids=""
cleanup() {
  for pid in $background_pids; do
    kill -TERM "$pid" 2>/dev/null || true
  done
  if [[ ${KEEP_REWATCH_WATCH_AUDIT:-0} == 1 ]]; then
    echo "Kept watch audit workdir: $work_root" >&2
  else
    find "$work_root" -depth -delete
  fi
}
trap cleanup EXIT INT TERM

marker_script="$work_root/mark-build.mjs"
printf '%s\n' \
  'import fs from "node:fs";' \
  'fs.appendFileSync(process.env.REWATCH_WATCH_AUDIT_MARKER, `${Date.now()}\n`);' \
  >"$marker_script"

wait_for_lines() {
  local path=$1 expected=$2
  for _ in $(seq 1 400); do
    if [[ -f "$path" ]] && [[ $(wc -l <"$path") -ge $expected ]]; then
      return
    fi
    sleep 0.05
  done
  echo "Timed out waiting for $expected build markers in $path" >&2
  exit 1
}

wait_for_watch_ready() {
  local trace_prefix=$1 source_directory=$2
  for _ in $(seq 1 400); do
    if grep -hF "inotify_add_watch" "$trace_prefix".* 2>/dev/null \
      | grep -Fq "\"$source_directory\""; then
      return
    fi
    sleep 0.05
  done
  echo "Timed out waiting for a native watch on $source_directory" >&2
  exit 1
}

trace_watch_edit() {
  local implementation=$1 executable=$2
  local fixture="$work_root/$implementation"
  local marker="$work_root/$implementation.marker"
  local trace_prefix="$work_root/$implementation-edit.file"
  local normalized="$work_root/$implementation-edit"
  cp -R "$repo_root/rewatch-ocaml/tests/basic" "$fixture"
  (
    cd "$fixture"
    REWATCH_WATCH_AUDIT_MARKER="$marker" \
      strace -f -ff -qq -ttt -yy -s 4096 -e trace=%file,getdents64 \
        -o "$trace_prefix" "$executable" watch \
        --after-build "node $marker_script" . \
        >"$normalized.stdout" 2>"$normalized.stderr"
  ) &
  local trace_pid=$!
  background_pids="$background_pids $trace_pid"
  wait_for_lines "$marker" 1
  wait_for_watch_ready "$trace_prefix" "$fixture/src"
  local start_epoch end_epoch
  start_epoch=$(date +%s.%N)
  printf 'let answer = A.value + 2\n' >"$fixture/src/B.res"
  wait_for_lines "$marker" 2
  end_epoch=$(date +%s.%N)
  rm -f "$fixture/lib/watch.lock"
  wait "$trace_pid"
  background_pids=${background_pids% $trace_pid}
  node "$normalizer" "$trace_prefix" "$fixture" "$normalized" \
    "$start_epoch" "$end_epoch"
}

trace_watch_edit rust "$rust_executable"
trace_watch_edit ocaml "$ocaml_executable"

echo "watch single-edit project filesystem categories (Rust / OCaml)"
join -a 1 -a 2 -e 0 -o 0,1.2,2.2 \
  "$work_root/rust-edit.categories.tsv" \
  "$work_root/ocaml-edit.categories.tsv"
for implementation in rust ocaml; do
  echo
  echo "$implementation most repeated project path operations"
  sort -t $'\t' -k1,1nr "$work_root/$implementation-edit.paths.tsv" \
    | sed -n '1,20p'
done

echo
echo "This audit covers one edit inside an already-running watcher. Inspect"
echo "retained traces for per-process attribution and exact repeated paths."
