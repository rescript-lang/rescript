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

for command in basename cp dirname find git join mktemp node sed sort strace tar; do
  command -v "$command" >/dev/null || {
    echo "Missing required command: $command" >&2
    exit 2
  }
done
if [[ ! -x "$rust_executable" || ! -x "$ocaml_executable" ]]; then
  echo "Both rewatch executables must exist and be executable." >&2
  exit 2
fi

work_root=$(mktemp -d "${TMPDIR:-/tmp}/rewatch-filesystem-audit.XXXXXX")
cleanup() {
  if [[ ${KEEP_REWATCH_FILESYSTEM_AUDIT:-0} == 1 ]]; then
    echo "Kept filesystem audit workdir: $work_root" >&2
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
  while IFS= read -r dependency_tree; do
    local relative_tree=${dependency_tree#"$repo_root/"}
    mkdir -p "$(dirname "$destination/$relative_tree")"
    cp -a --reflink=auto "$dependency_tree" "$destination/$relative_tree"
  done < <(find "$repo_root/rewatch/testrepo" -type d -name node_modules \
    -prune -print)
}

if [[ -z ${RESCRIPT_BSC_EXE:-} || -z ${RESCRIPT_RUNTIME:-} ]]; then
  eval "$(cd "$repo_root/rewatch/tests" && node ./get_bin_paths.js)"
fi
export RESCRIPT_BSC_EXE RESCRIPT_RUNTIME

rust_root="$work_root/rust"
ocaml_root="$work_root/ocaml"
prepare_fixture "$rust_root"
prepare_fixture "$ocaml_root"
rust_fixture="$rust_root/rewatch/testrepo"
ocaml_fixture="$ocaml_root/rewatch/testrepo"

trace_build() {
  local implementation=$1 scenario=$2 executable=$3 fixture=$4 clean_first=$5
  local trace_prefix="$work_root/$implementation-$scenario.file"
  local normalized="$work_root/$implementation-$scenario"
  if [[ "$clean_first" == 1 ]]; then
    "$executable" clean "$fixture" >/dev/null 2>&1
  fi
  (
    cd "$fixture"
    strace -f -ff -qq -yy -s 4096 -e trace=%file,getdents64 \
      -o "$trace_prefix" "$executable" build . \
      >"$normalized.stdout" 2>"$normalized.stderr"
  )
  node "$normalizer" "$trace_prefix" "$fixture" "$normalized"
}

for scenario in clean unchanged; do
  clean_first=0
  [[ "$scenario" == clean ]] && clean_first=1
  trace_build rust "$scenario" "$rust_executable" "$rust_fixture" "$clean_first"
  trace_build ocaml "$scenario" "$ocaml_executable" "$ocaml_fixture" "$clean_first"
done

printf '\n// filesystem audit single edit\n' \
  >>"$rust_fixture/packages/watch-warnings/src/B.res"
printf '\n// filesystem audit single edit\n' \
  >>"$ocaml_fixture/packages/watch-warnings/src/B.res"
trace_build rust edit "$rust_executable" "$rust_fixture" 0
trace_build ocaml edit "$ocaml_executable" "$ocaml_fixture" 0

for scenario in clean unchanged edit; do
  echo
  echo "$scenario project filesystem categories (Rust / OCaml)"
  join -a 1 -a 2 -e 0 -o 0,1.2,2.2 \
    "$work_root/rust-$scenario.categories.tsv" \
    "$work_root/ocaml-$scenario.categories.tsv"
  echo "$scenario most repeated OCaml project path operations"
  sort -t $'\t' -k1,1nr "$work_root/ocaml-$scenario.paths.tsv" | sed -n '1,20p'
done

echo
echo "This audit is diagnostic: inspect repeated project-local accesses and the"
echo "retained manifests; raw Rust/OCaml totals are not an equivalence gate."
