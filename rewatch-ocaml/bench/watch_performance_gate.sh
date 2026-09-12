#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 2 || $# -gt 3 ]]; then
  echo "Usage: $0 RUST_REWATCH OCAML_REWATCH [RUNS]" >&2
  exit 2
fi

repo_root=$(cd "$(dirname "$0")/../.." && pwd)
rust_executable=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
ocaml_executable=$(cd "$(dirname "$2")" && pwd)/$(basename "$2")
runs=${3:-7}
threshold_percent=${REWATCH_WATCH_PERFORMANCE_THRESHOLD_PERCENT:-150}

if ((runs < 5 || runs % 2 == 0)); then
  echo "RUNS must be an odd number of at least five." >&2
  exit 2
fi
for command in awk cat cmp cp date find grep mktemp node sed seq setsid sleep sort tail wc; do
  command -v "$command" >/dev/null || {
    echo "Missing required command: $command" >&2
    exit 2
  }
done
if [[ ! -x $rust_executable || ! -x $ocaml_executable ]]; then
  echo "Both rewatch executables must exist and be executable." >&2
  exit 2
fi
if [[ ! -d /proc/self/fd ]]; then
  echo "The retained-watch performance gate requires Linux /proc." >&2
  exit 2
fi

if [[ -z ${RESCRIPT_BSC_EXE:-} || -z ${RESCRIPT_RUNTIME:-} ]]; then
  eval "$(cd "$repo_root/rewatch/tests" && node ./get_bin_paths.js)"
fi
real_bsc=$RESCRIPT_BSC_EXE
runtime=$RESCRIPT_RUNTIME
counting_bsc="$repo_root/_build/default/tests/rewatch_ounit_tests/rewatch_bsc_test_proxy.exe"

work_root=$(mktemp -d "${TMPDIR:-/tmp}/rewatch-watch-performance.XXXXXX")
declare -A pids=()
terminate_group() {
  local pid=$1
  local reaped=false state
  if ! kill -TERM -- "-$pid" 2>/dev/null; then
    kill -TERM "$pid" 2>/dev/null || true
  fi
  for _ in $(seq 1 100); do
    if [[ $reaped == false && -r /proc/$pid/stat ]]; then
      state=$(awk '{print $3}' "/proc/$pid/stat" 2>/dev/null || true)
      if [[ $state == Z ]]; then
        wait "$pid" 2>/dev/null || true
        reaped=true
      fi
    fi
    if ! kill -0 -- "-$pid" 2>/dev/null; then
      if [[ $reaped == false ]]; then wait "$pid" 2>/dev/null || true; fi
      return
    fi
    sleep 0.05
  done
  kill -KILL -- "-$pid" 2>/dev/null || true
  if [[ $reaped == false ]]; then kill -KILL "$pid" 2>/dev/null || true; fi
  if [[ $reaped == false ]]; then wait "$pid" 2>/dev/null || true; fi
}
cleanup() {
  trap - EXIT INT TERM
  local implementation pid
  for implementation in "${!pids[@]}"; do
    pid=${pids[$implementation]}
    terminate_group "$pid"
  done
  if [[ ${KEEP_REWATCH_WATCH_PERFORMANCE:-0} == 1 ]]; then
    echo "Kept watch performance workdir: $work_root" >&2
  else
    find "$work_root" -depth -delete
  fi
}
trap cleanup EXIT INT TERM

marker_script="$work_root/mark-build.mjs"
printf '%s\n' \
  'import fs from "node:fs";' \
  'fs.appendFileSync(process.env.REWATCH_WATCH_MARKER, `${Date.now()}\n`);' \
  >"$marker_script"

wait_for_lines() {
  local path=$1 expected=$2
  for _ in $(seq 1 400); do
    if [[ -f $path ]] && [[ $(wc -l <"$path") -ge $expected ]]; then return; fi
    sleep 0.05
  done
  echo "Timed out waiting for $expected lines in $path" >&2
  exit 1
}

wait_for_text_count() {
  local path=$1 text=$2 expected=$3
  for _ in $(seq 1 400); do
    if [[ -f $path ]] && \
      [[ $(grep -cF "$text" "$path" 2>/dev/null || true) -ge $expected ]]; then
      return
    fi
    sleep 0.05
  done
  echo "Timed out waiting for occurrence $expected of '$text' in $path" >&2
  exit 1
}

wait_for_idle() {
  local pid=$1
  for _ in $(seq 1 400); do
    if [[ ! -d /proc/$pid ]]; then
      echo "Watcher $pid exited before becoming idle." >&2
      exit 1
    fi
    local children state
    children=$(cat "/proc/$pid/task/$pid/children" 2>/dev/null || true)
    state=$(awk '{print $3}' "/proc/$pid/stat")
    if [[ -z $children && $state == S ]]; then return; fi
    sleep 0.01
  done
  echo "Timed out waiting for watcher $pid to become idle." >&2
  exit 1
}

resource_value() {
  local pid=$1 kind=$2
  case $kind in
    fd) find "/proc/$pid/fd" -mindepth 1 -maxdepth 1 | wc -l ;;
    tasks) find "/proc/$pid/task" -mindepth 1 -maxdepth 1 -type d | wc -l ;;
    rss) awk '/^VmRSS:/ {print $2}' "/proc/$pid/status" ;;
  esac
}

start_watcher() {
  local implementation=$1 executable=$2 fixture
  fixture="$work_root/$implementation"
  cp -R "$repo_root/rewatch-ocaml/tests/basic" "$fixture"
  : >"$work_root/$implementation.bsc"
  setsid env \
    RESCRIPT_BSC_EXE="$counting_bsc" \
    REWATCH_BSC_PROXY_MODE=counting \
    RESCRIPT_RUNTIME="$runtime" \
    REWATCH_REAL_BSC="$real_bsc" \
    REWATCH_BSC_CALL_LOG="$work_root/$implementation.bsc" \
    REWATCH_WATCH_MARKER="$work_root/$implementation.marker" \
    "$executable" watch --after-build "node $marker_script" "$fixture" \
    >"$work_root/$implementation.stdout" \
    2>"$work_root/$implementation.stderr" &
  pids[$implementation]=$!
  wait_for_lines "$work_root/$implementation.marker" 1
  wait_for_text_count "$work_root/$implementation.stdout" \
    "Finished initial compilation" 1
  wait_for_idle "${pids[$implementation]}"
}

start_watcher rust "$rust_executable"
start_watcher ocaml "$ocaml_executable"

# One unmeasured edit pays any first-incremental initialization cost before the
# retained-state samples begin.
for implementation in rust ocaml; do
  printf 'let answer = A.value + 1\n// warm retained edit\n' \
    >"$work_root/$implementation/src/B.res"
  wait_for_lines "$work_root/$implementation.marker" 2
  wait_for_text_count "$work_root/$implementation.stdout" \
    "Finished incremental compilation" 1
  wait_for_idle "${pids[$implementation]}"
  : >"$work_root/$implementation.bsc"
done

declare -A baseline_fd baseline_tasks baseline_rss max_fd max_tasks max_rss
for implementation in rust ocaml; do
  pid=${pids[$implementation]}
  baseline_fd[$implementation]=$(resource_value "$pid" fd)
  baseline_tasks[$implementation]=$(resource_value "$pid" tasks)
  baseline_rss[$implementation]=$(resource_value "$pid" rss)
  max_fd[$implementation]=${baseline_fd[$implementation]}
  max_tasks[$implementation]=${baseline_tasks[$implementation]}
  max_rss[$implementation]=${baseline_rss[$implementation]}
  : >"$work_root/$implementation.latencies"
done

measure_edit() {
  local implementation=$1 round=$2 expected=$((round + 2))
  local started finished latency pid value
  started=$(date +%s%3N)
  printf 'let answer = A.value + 1\n// retained edit %d\n' "$round" \
    >"$work_root/$implementation/src/B.res"
  wait_for_lines "$work_root/$implementation.marker" "$expected"
  finished=$(tail -n 1 "$work_root/$implementation.marker")
  latency=$((finished - started))
  printf '%d\n' "$latency" >>"$work_root/$implementation.latencies"
  wait_for_text_count "$work_root/$implementation.stdout" \
    "Finished incremental compilation" "$((round + 1))"
  wait_for_idle "${pids[$implementation]}"
  pid=${pids[$implementation]}
  for kind in fd tasks rss; do
    value=$(resource_value "$pid" "$kind")
    case $kind in
      fd) if ((value > max_fd[$implementation])); then max_fd[$implementation]=$value; fi ;;
      tasks) if ((value > max_tasks[$implementation])); then max_tasks[$implementation]=$value; fi ;;
      rss) if ((value > max_rss[$implementation])); then max_rss[$implementation]=$value; fi ;;
    esac
  done
}

for round in $(seq 1 "$runs"); do
  if ((round % 2 == 1)); then
    measure_edit rust "$round"
    measure_edit ocaml "$round"
  else
    measure_edit ocaml "$round"
    measure_edit rust "$round"
  fi
  if ! cmp -s "$work_root/rust/src/B.mjs" "$work_root/ocaml/src/B.mjs"; then
    echo "Generated output differs after retained edit $round." >&2
    exit 1
  fi
done

median() {
  sort -n "$1" | sed -n "$((runs / 2 + 1))p"
}
rust_median=$(median "$work_root/rust.latencies")
ocaml_median=$(median "$work_root/ocaml.latencies")

normalize_calls() {
  local implementation=$1
  sed "s#$work_root/$implementation#<ROOT>#g" \
    "$work_root/$implementation.bsc" >"$work_root/$implementation.bsc.normalized"
}
normalize_calls rust
normalize_calls ocaml
if ! cmp -s "$work_root/rust.bsc.normalized" \
  "$work_root/ocaml.bsc.normalized"; then
  echo "Rust and OCaml retained edits performed different compiler work." >&2
  exit 1
fi
for implementation in rust ocaml; do
  parse_count=$(grep -cF -- '-bs-ast' "$work_root/$implementation.bsc" || true)
  total_count=$(wc -l <"$work_root/$implementation.bsc")
  if ((parse_count != runs || total_count != runs * 2)); then
    printf '%s retained work was %d parser / %d total calls; expected %d / %d.\n' \
      "$implementation" "$parse_count" "$total_count" "$runs" "$((runs * 2))" >&2
    exit 1
  fi
done

for implementation in rust ocaml; do
  pid=${pids[$implementation]}
  final_fd=$(resource_value "$pid" fd)
  final_tasks=$(resource_value "$pid" tasks)
  final_rss=$(resource_value "$pid" rss)
  if ((max_fd[$implementation] > baseline_fd[$implementation] + 4 ||
       max_tasks[$implementation] > baseline_tasks[$implementation] + 2 ||
       max_rss[$implementation] > baseline_rss[$implementation] + 16384)); then
    echo "$implementation watcher resource growth exceeded the retained gate." >&2
    exit 1
  fi
  printf '%-5s resources: fd %d→%d (max %d), tasks %d→%d (max %d), RSS %d→%d KiB (max %d)\n' \
    "$implementation" "${baseline_fd[$implementation]}" "$final_fd" \
    "${max_fd[$implementation]}" "${baseline_tasks[$implementation]}" \
    "$final_tasks" "${max_tasks[$implementation]}" \
    "${baseline_rss[$implementation]}" "$final_rss" "${max_rss[$implementation]}"
done

if ((ocaml_median * 100 > rust_median * threshold_percent)); then
  printf 'OCaml retained-watch median %d ms exceeds %d%% of Rust median %d ms.\n' \
    "$ocaml_median" "$threshold_percent" "$rust_median" >&2
  exit 1
fi

printf 'retained-watch median: Rust %d ms, OCaml %d ms (limit %d%%)\n' \
  "$rust_median" "$ocaml_median" "$threshold_percent"
printf 'compiler work: %d parser and %d compiler calls per implementation\n' \
  "$runs" "$runs"

for implementation in rust ocaml; do
  rm -f "$work_root/$implementation/lib/watch.lock"
  wait "${pids[$implementation]}"
  unset "pids[$implementation]"
done

echo "Retained-watch performance, work, output, and resource gates passed"
