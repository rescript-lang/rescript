#!/bin/bash
set -eu

root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
rust=${1:-$root/rewatch/target/debug/rescript}
ocaml=${2:-$root/_build/default/rewatch-ocaml/rescript_ocaml.exe}
rust=$(realpath "$rust")
ocaml=$(realpath "$ocaml")
work=$(mktemp -d "${TMPDIR:-/tmp}/rewatch-interactive-output-XXXXXX")
trap 'rm -rf "$work"' EXIT

if ! command -v script >/dev/null 2>&1; then
  echo "Interactive output gate requires the util-linux script command" >&2
  exit 1
fi

for implementation in rust ocaml; do
  mkdir -p "$work/$implementation/src"
  printf '{"name":"interactive-output","sources":["src"]}\n' \
    >"$work/$implementation/rescript.json"
  printf 'let value = 1\n' >"$work/$implementation/src/A.res"
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

echo "Interactive output phases matched"
