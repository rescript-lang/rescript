#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 2 || $# -gt 3 ]]; then
  echo "Usage: $0 RUST_REWATCH OCAML_REWATCH [RUNS]" >&2
  exit 2
fi

repo_root=$(cd "$(dirname "$0")/../.." && pwd)
rust_executable=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
ocaml_executable=$(cd "$(dirname "$2")" && pwd)/$(basename "$2")
runs=${3:-5}
threshold_percent=${REWATCH_PERFORMANCE_THRESHOLD_PERCENT:-125}

if [[ ! -x "$rust_executable" || ! -x "$ocaml_executable" ]]; then
  echo "Both rewatch executables must exist and be executable." >&2
  exit 2
fi
if [[ ! "$runs" =~ ^[1-9][0-9]*$ || $((runs % 2)) -eq 0 ]]; then
  echo "RUNS must be a positive odd integer so the median is unambiguous." >&2
  exit 2
fi
if ((runs < 5)) && [[ ${REWATCH_ALLOW_SMOKE_RUN:-0} != 1 ]]; then
  echo "RUNS must be at least 5 for the quality gate." >&2
  echo "Set REWATCH_ALLOW_SMOKE_RUN=1 only for a non-authoritative smoke run." >&2
  exit 2
fi
for command in awk basename cmp cp date diff dirname find getconf git grep head \
  mktemp node ps sed sha256sum sleep sort strace tar uname xargs; do
  command -v "$command" >/dev/null || {
    echo "Missing required command: $command" >&2
    exit 2
  }
done
if [[ ! -d /proc ]]; then
  echo "This gate requires Linux /proc for process-tree RSS sampling." >&2
  exit 2
fi

work_root=$(mktemp -d "${TMPDIR:-/tmp}/rewatch-performance.XXXXXX")
cleanup() {
  if [[ ${KEEP_REWATCH_BENCHMARK_WORKDIR:-0} == 1 ]]; then
    echo "Kept benchmark workdir: $work_root" >&2
  else
    find "$work_root" -depth -delete
  fi
}
trap cleanup EXIT INT TERM

prepare_fixture() {
  local destination=$1
  mkdir -p "$destination"
  git -C "$repo_root" archive HEAD \
    rewatch/testrepo packages/@rescript/belt packages/@rescript/runtime \
    | tar -x -C "$destination"
  # Dependencies and workspace links are intentionally ignored by Git. Keep a
  # separate installed tree in each root so neither implementation can affect
  # the other's generated dependency artifacts.
  while IFS= read -r dependency_tree; do
    local relative_tree=${dependency_tree#"$repo_root/"}
    mkdir -p "$(dirname "$destination/$relative_tree")"
    cp -a --reflink=auto "$dependency_tree" "$destination/$relative_tree"
  done < <(find "$repo_root/rewatch/testrepo" -type d -name node_modules \
    -prune -print)
}

rust_root="$work_root/rust"
ocaml_root="$work_root/ocaml"
prepare_fixture "$rust_root"
prepare_fixture "$ocaml_root"
rust_fixture="$rust_root/rewatch/testrepo"
ocaml_fixture="$ocaml_root/rewatch/testrepo"

eval "$(cd "$repo_root/rewatch/tests" && node ./get_bin_paths.js)"
export RESCRIPT_BSC_EXE RESCRIPT_RUNTIME

results="$work_root/results.csv"
echo "implementation,iteration,wall_ms,peak_tree_rss_kib" >"$results"

tree_rss_kib() {
  local root_pid=$1
  ps -e -o pid=,ppid=,rss= | awk -v root="$root_pid" '
    { pids[NR] = $1; parent[$1] = $2; memory[$1] = $3 }
    END {
      live[root] = 1
      for (pass = 0; pass < NR; pass++)
        for (i = 1; i <= NR; i++)
          if (live[parent[pids[i]]]) live[pids[i]] = 1
      for (pid in live) total += memory[pid]
      print total + 0
    }'
}

clean_and_build() {
  local executable=$1 fixture=$2 output=$3
  "$executable" clean "$fixture" >/dev/null 2>&1
  "$executable" build "$fixture" >"$output" 2>"$output.stderr"
}

measure() {
  local implementation=$1 executable=$2 fixture=$3 iteration=$4
  local output="$work_root/${implementation}-${iteration}"
  "$executable" clean "$fixture" >/dev/null 2>&1
  local start_ns root_pid peak=0 rss end_ns wall_ms
  start_ns=$(date +%s%N)
  "$executable" build "$fixture" >"$output" 2>"$output.stderr" &
  root_pid=$!
  while kill -0 "$root_pid" 2>/dev/null; do
    rss=$(tree_rss_kib "$root_pid")
    if ((rss > peak)); then
      peak=$rss
    fi
    sleep 0.02
  done
  wait "$root_pid"
  end_ns=$(date +%s%N)
  wall_ms=$(((end_ns - start_ns) / 1000000))
  echo "$implementation,$iteration,$wall_ms,$peak" >>"$results"
  printf '%-5s run %d: %6d ms  %8d KiB\n' \
    "$implementation" "$iteration" "$wall_ms" "$peak"
}

median_column() {
  local implementation=$1 column=$2 middle=$((runs / 2 + 1))
  awk -F, -v implementation="$implementation" \
    '$1 == implementation { print $'"$column"' }' "$results" \
    | sort -n | sed -n "${middle}p"
}

echo "Rewatch clean-build performance gate"
echo "commit: $(git -C "$repo_root" rev-parse HEAD)"
echo "host: $(uname -a)"
echo "cpus: $(getconf _NPROCESSORS_ONLN 2>/dev/null || echo unknown)"
echo "runs: $runs (interleaved after one warm-up each)"
echo "threshold: ${threshold_percent}% of Rust median wall and RSS"

clean_and_build "$rust_executable" "$rust_fixture" "$work_root/rust-warmup"
clean_and_build "$ocaml_executable" "$ocaml_fixture" "$work_root/ocaml-warmup"

for ((iteration = 1; iteration <= runs; iteration++)); do
  if ((iteration % 2 == 1)); then
    measure rust "$rust_executable" "$rust_fixture" "$iteration"
    measure ocaml "$ocaml_executable" "$ocaml_fixture" "$iteration"
  else
    measure ocaml "$ocaml_executable" "$ocaml_fixture" "$iteration"
    measure rust "$rust_executable" "$rust_fixture" "$iteration"
  fi
done

rust_wall=$(median_column rust 3)
ocaml_wall=$(median_column ocaml 3)
rust_rss=$(median_column rust 4)
ocaml_rss=$(median_column ocaml 4)
printf 'median Rust:  %6d ms  %8d KiB\n' "$rust_wall" "$rust_rss"
printf 'median OCaml: %6d ms  %8d KiB\n' "$ocaml_wall" "$ocaml_rss"

trace_and_classify() {
  local implementation=$1 executable=$2 fixture=$3 manifest=$4
  local trace_prefix="$work_root/${implementation}.execve"
  "$executable" clean "$fixture" >/dev/null 2>&1
  strace -f -ff -qq -s 4096 -e trace=execve,chdir -o "$trace_prefix" \
    "$executable" build "$fixture" \
    >"$work_root/${implementation}-trace.out" \
    2>"$work_root/${implementation}-trace.stderr"
  local trace_files=("$trace_prefix".*)
  local implementation_root=${fixture%/rewatch/testrepo}
  local trace_file exec_line argv cwd_line cwd phase input identity
  : >"$manifest.unsorted"
  for trace_file in "${trace_files[@]}"; do
    exec_line=$(grep -m1 -E \
      'execve\("[^"]*(bsc\.exe|sury-ppx)' "$trace_file" || true)
    if [[ -z "$exec_line" ]]; then
      continue
    fi
    argv=${exec_line#*, }
    argv=${argv%%], 0x*}]
    cwd_line=$(grep -m1 '^chdir("' "$trace_file" || true)
    cwd=${cwd_line#chdir(\"}
    cwd=${cwd%%\"*}
    if [[ "$exec_line" == *bsc.exe* ]]; then
      if [[ "$argv" == *'"-bs-ast"'* ]]; then
        phase=parse
      elif [[ "$argv" == *'.mlmap"'* ]]; then
        phase=namespace
      else
        phase=compile
      fi
      input=${argv##*, }
      input=${input%]}
      identity="$cwd"$'\t'"$phase"$'\t'"$input"
    else
      # PPX temporary input/output names are deliberately randomized. Its
      # executable identity and count are the stable unit of work.
      identity=ppx$'\t'"${argv%%,*}"
    fi
    printf '%s\n' "$identity" \
      | sed "s#$implementation_root#<ROOT>#g" >>"$manifest.unsorted"
  done
  sort "$manifest.unsorted" >"$manifest"
  local invocations parse namespace compile interface ppx
  invocations=$(grep -hE -c 'execve\("[^"]*bsc\.exe"' "${trace_files[@]}" \
    | awk '{ total += $1 } END { print total + 0 }')
  parse=$(grep -hE 'execve\("[^"]*bsc\.exe"' "${trace_files[@]}" \
    | grep -F -c '"-bs-ast"' || true)
  namespace=$(grep -hE 'execve\("[^"]*bsc\.exe"' "${trace_files[@]}" \
    | grep -E -c '\.mlmap"' || true)
  interface=$(grep -hE 'execve\("[^"]*bsc\.exe"' "${trace_files[@]}" \
    | grep -vF '"-bs-ast"' | grep -vE '\.mlmap"' \
    | grep -E -c '\.iast"' || true)
  compile=$((invocations - parse - namespace))
  ppx=$(grep -hE -c 'execve\("[^"]*sury-ppx' "${trace_files[@]}" \
    | awk '{ total += $1 } END { print total + 0 }')
  echo "$invocations,$parse,$namespace,$compile,$interface,$ppx"
}

rust_invocations="$work_root/rust-invocations.txt"
ocaml_invocations="$work_root/ocaml-invocations.txt"
rust_work=$(trace_and_classify rust "$rust_executable" "$rust_fixture" \
  "$rust_invocations")
ocaml_work=$(trace_and_classify ocaml "$ocaml_executable" "$ocaml_fixture" \
  "$ocaml_invocations")
echo "work columns: bsc_total,parse,namespace,compile,interfaces,ppx"
echo "work Rust:  $rust_work"
echo "work OCaml: $ocaml_work"

artifact_manifest() {
  local root=$1 output=$2
  find "$root" -type f \
    \( -name '*.cmi' -o -name '*.cmj' -o -name '*.mlmap' \
       -o -name '*.js' -o -name '*.mjs' -o -name '*.cjs' -o -name '*.map' \) \
    ! -path '*/node_modules/*' ! -name '.compiler.log' \
    ! -name build.ninja ! -name compiler-info.json -print0 \
    | sort -z | xargs -0 sha256sum | sed "s#$root/##" >"$output"
}

# Use the same absolute path for both builds so paths embedded in binary
# compiler artifacts are directly comparable byte for byte.
equivalence_root="$work_root/equivalence"
prepare_fixture "$equivalence_root"
equivalence_fixture="$equivalence_root/rewatch/testrepo"
rust_artifacts="$work_root/rust-artifacts.sha256"
ocaml_artifacts="$work_root/ocaml-artifacts.sha256"
clean_and_build "$rust_executable" "$equivalence_fixture" \
  "$work_root/rust-equivalence"
artifact_manifest "$equivalence_root" "$rust_artifacts"
# Recreate, rather than clean, the fixture so OCaml cannot inherit an artifact
# that only Rust produced. Reusing the same pathname keeps embedded paths equal.
find "$equivalence_root" -depth -delete
prepare_fixture "$equivalence_root"
clean_and_build "$ocaml_executable" "$equivalence_fixture" \
  "$work_root/ocaml-equivalence"
artifact_manifest "$equivalence_root" "$ocaml_artifacts"
if cmp -s "$rust_artifacts" "$ocaml_artifacts"; then
  artifact_equivalence=1
  echo "artifacts: identical generated file sets and contents"
else
  artifact_equivalence=0
  echo "artifact manifest diff:" >&2
  diff -u "$rust_artifacts" "$ocaml_artifacts" >&2 || true
fi

failed=0
if ((ocaml_wall * 100 > rust_wall * threshold_percent)); then
  echo "FAIL: OCaml median wall time exceeds the threshold." >&2
  failed=1
fi
if ((ocaml_rss * 100 > rust_rss * threshold_percent)); then
  echo "FAIL: OCaml median peak tree RSS exceeds the threshold." >&2
  failed=1
fi
if [[ "$rust_work" != "$ocaml_work" ]]; then
  echo "FAIL: Rust and OCaml performed different compiler work." >&2
  failed=1
fi
if ! cmp -s "$rust_invocations" "$ocaml_invocations"; then
  echo "FAIL: Rust and OCaml performed different module/PPX work." >&2
  diff -u "$rust_invocations" "$ocaml_invocations" >&2 || true
  failed=1
fi
if ((artifact_equivalence == 0)); then
  echo "FAIL: Rust and OCaml generated different artifacts." >&2
  failed=1
fi

if ((failed)); then
  exit 1
fi
if ((runs < 5)); then
  echo "PASS: correctness smoke checks passed; performance gate not evaluated."
else
  echo "PASS: timing, memory, compiler-work, and artifact-equivalence gates passed."
fi
