#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 || $# -gt 2 ]]; then
  echo "Usage: $0 OUTPUT_DIRECTORY [ODD_RUN_COUNT]" >&2
  exit 2
fi

repo_root=$(cd "$(dirname "$0")/../.." && pwd)
results=$(mkdir -p "$1" && cd "$1" && pwd)
runs=${2:-5}
if [[ ! $runs =~ ^[1-9][0-9]*$ || $((runs % 2)) -eq 0 ]]; then
  echo "Run count must be positive and odd." >&2
  exit 2
fi

fixture_root="$results/fixture"
if [[ -e "$fixture_root" ]]; then
  echo "Output directory already contains a fixture: $fixture_root" >&2
  exit 2
fi
mkdir -p "$fixture_root"
git -c "safe.directory=$repo_root" -C "$repo_root" archive HEAD \
  rewatch/testrepo packages/@rescript/belt packages/@rescript/runtime \
  | tar -x -C "$fixture_root"
while IFS= read -r dependency_tree; do
  relative_tree=${dependency_tree#"$repo_root/"}
  mkdir -p "$(dirname "$fixture_root/$relative_tree")"
  cp -a --reflink=auto "$dependency_tree" "$fixture_root/$relative_tree"
done < <(find "$repo_root/rewatch/testrepo" -type d -name node_modules -prune -print)
node "$repo_root/rewatch/tests/add-belt-dependencies.mjs" \
  "$fixture_root/rewatch/testrepo"

export RESCRIPT_RUNTIME="$repo_root/packages/@rescript/runtime"
export REWATCH_COMPILER_DOMAINS=${REWATCH_COMPILER_DOMAINS:-8}
plain_executable=${REWATCH_PLAIN_EXECUTABLE:-"$repo_root/_build/default/rewatch-ocaml/rescript_ocaml.exe"}
plain_bsc=${REWATCH_PLAIN_BSC:-"$repo_root/_build/default/compiler/bsc/rescript_compiler_main.exe"}
traced_executable="$repo_root/_build/default/rewatch-ocaml/rescript_ocaml.exe"
traced_bsc="$repo_root/_build/default/compiler/bsc/rescript_compiler_main.exe"
mkdir -p "$results/bin"
cp "$plain_executable" "$results/bin/rewatch-plain"
cp "$plain_bsc" "$results/bin/bsc-plain"
cp "$traced_executable" "$results/bin/rewatch-traced"
cp "$traced_bsc" "$results/bin/bsc-traced"
fixture="$fixture_root/rewatch/testrepo"

{
  printf 'fixture_commit=%s\n' "$(git -c "safe.directory=$repo_root" -C "$repo_root" rev-parse HEAD)"
  printf 'host=%s\n' "$(uname -a)"
  printf 'workers=%s\n' "$REWATCH_COMPILER_DOMAINS"
  printf 'runtime=%s\n' "$RESCRIPT_RUNTIME"
  sha256sum "$results/bin/rewatch-plain" "$results/bin/rewatch-traced" \
    "$results/bin/bsc-plain" "$results/bin/bsc-traced"
} >"$results/metadata.txt"

for ((iteration = 1; iteration <= runs; iteration++)); do
  for slot in 0 1; do
    if (((iteration + slot) % 2 == 0)); then
      mode=traced
    else
      mode=plain
    fi
    executable="$results/bin/rewatch-$mode"
    export RESCRIPT_BSC_EXE="$results/bin/bsc-$mode"
    # Belt lives outside testrepo and is not removed by cleaning that project.
    # Clean it explicitly so every sample recompiles the same 1,031 requests.
    "$executable" clean "$fixture_root/packages/@rescript/belt" \
      >"$results/$mode-$iteration.belt-clean.log" 2>&1
    "$executable" clean "$fixture" >"$results/$mode-$iteration.clean.log" 2>&1
    trace="$results/$mode-$iteration.trace.tsv"
    if [[ $mode == traced ]]; then
      REWATCH_TYPECHECK_TRACE="$trace" /usr/bin/time \
        -f 'elapsed_s=%e user_s=%U sys_s=%S peak_rss_kib=%M' \
        -o "$results/$mode-$iteration.time" \
        "$executable" build "$fixture" >"$results/$mode-$iteration.build.log" 2>&1
    else
      env -u REWATCH_TYPECHECK_TRACE /usr/bin/time \
        -f 'elapsed_s=%e user_s=%U sys_s=%S peak_rss_kib=%M' \
        -o "$results/$mode-$iteration.time" \
        "$executable" build "$fixture" >"$results/$mode-$iteration.build.log" 2>&1
    fi
    printf '%s run %d: ' "$mode" "$iteration"
    cat "$results/$mode-$iteration.time"
  done
done
