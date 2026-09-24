#!/bin/bash
set -eu

root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
rust=${1:-$root/rewatch/target/debug/rescript}
ocaml=${2:-$root/_build/default/rewatch-ocaml/rescript_ocaml.exe}
rust=$(realpath "$rust")
ocaml=$(realpath "$ocaml")
work=$(mktemp -d "${TMPDIR:-/tmp}/rewatch-verbose-output-XXXXXX")
cleanup() {
  rm -rf "$work"
}
trap cleanup EXIT

for implementation in rust-v ocaml-v rust-vv ocaml-vv; do
  mkdir -p "$work/$implementation/src"
  printf '{"name":"verbose-output","sources":["src"]}\n' \
    >"$work/$implementation/rescript.json"
  printf 'let value = 1\n' >"$work/$implementation/src/A.res"
  printf 'let value = A.value\n' >"$work/$implementation/src/B.res"
  printf 'let value = 2\n' >"$work/$implementation/src/Authored.res"
  printf 'let value: int\n' >"$work/$implementation/src/WithInterface.resi"
  printf 'let value = 3\n' >"$work/$implementation/src/WithInterface.res"
done

export RESCRIPT_BSC_EXE=${RESCRIPT_BSC_EXE:-$root/_build/default/compiler/bsc/rescript_compiler_main.exe}
export RESCRIPT_RUNTIME=${RESCRIPT_RUNTIME:-$root/packages/@rescript/runtime}

normalize_events() {
  input=$1
  output=$2
  sed -E \
    's# at ".*rescript\.json" for ".*"# at "<PROJECT>/rescript.json" for "<PROJECT>"#' \
    "$input" \
    | awk '
        /^(DEBUG|TRACE):$/ {
          label = $0
          if ((getline message) > 0) print label "\t" message
          next
        }
        /^compile dirty: / { print }
      ' \
    | sort >"$output"
}

capture() {
  implementation=$1
  executable=$2
  level=$3
  project="$work/$implementation"
  (
    cd "$project"
    "$executable" "$level" build >"$work/$implementation.stdout" \
      2>"$work/$implementation.stderr"
  )
  if [ -s "$work/$implementation.stderr" ]; then
    echo "$implementation unexpectedly wrote verbose build output to stderr" >&2
    cat "$work/$implementation.stderr" >&2
    exit 1
  fi
  normalize_events "$work/$implementation.stdout" \
    "$work/$implementation.events"
}

capture rust-v "$rust" -v
capture ocaml-v "$ocaml" -v
capture rust-vv "$rust" -vv
capture ocaml-vv "$ocaml" -vv

compare_events() {
  rust_events=$1
  ocaml_events=$2
  if ! cmp -s "$rust_events" "$ocaml_events"; then
    echo "Verbose semantic output differs" >&2
    printf '%s\n' '--- Rust events ---' >&2
    cat "$rust_events" >&2
    printf '%s\n' '--- OCaml events ---' >&2
    cat "$ocaml_events" >&2
    exit 1
  fi
}

compare_events "$work/rust-v.events" "$work/ocaml-v.events"
compare_events "$work/rust-vv.events" "$work/ocaml-vv.events"

if grep -E '^(TRACE:|compile dirty:)' "$work/ocaml-v.events" >/dev/null; then
  echo "-v unexpectedly emitted trace-level events" >&2
  cat "$work/ocaml-v.events" >&2
  exit 1
fi
if [ "$(grep -c '^compile dirty: ' "$work/ocaml-vv.events")" -ne 4 ]; then
  echo "-vv did not identify the four dirty modules" >&2
  cat "$work/ocaml-vv.events" >&2
  exit 1
fi
if ! grep -F $'TRACE:\tCompiled 4 out of 4 in the universe' \
  "$work/ocaml-vv.events" >/dev/null; then
  echo "-vv did not report the scheduler universe" >&2
  cat "$work/ocaml-vv.events" >&2
  exit 1
fi

capture_incremental() {
  implementation=$1
  executable=$2
  project="$work/$implementation"
  printf 'let value = 2\n' >"$project/src/A.res"
  (
    cd "$project"
    "$executable" -vv build >"$work/$implementation-incremental.stdout" \
      2>"$work/$implementation-incremental.stderr"
  )
  if [ -s "$work/$implementation-incremental.stderr" ]; then
    echo "$implementation incremental verbose output reached stderr" >&2
    cat "$work/$implementation-incremental.stderr" >&2
    exit 1
  fi
  normalize_events "$work/$implementation-incremental.stdout" \
    "$work/$implementation-incremental.events"
}

capture_incremental rust-vv "$rust"
capture_incremental ocaml-vv "$ocaml"
compare_events "$work/rust-vv-incremental.events" \
  "$work/ocaml-vv-incremental.events"
if [ "$(grep -c '^compile dirty: ' "$work/ocaml-vv-incremental.events")" -ne 1 ] \
  || ! grep -F $'TRACE:\tCompiled 2 out of 2 in the universe' \
    "$work/ocaml-vv-incremental.events" >/dev/null; then
  echo "Incremental -vv output lost its dirty/universe boundary" >&2
  cat "$work/ocaml-vv-incremental.events" >&2
  exit 1
fi

echo "Verbose semantic output matched"
