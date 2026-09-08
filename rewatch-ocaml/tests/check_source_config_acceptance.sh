#!/bin/bash
set -eu

root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
rust=${1:-$root/rewatch/target/debug/rescript}
ocaml=${2:-$root/_build/default/rewatch-ocaml/rescript_ocaml.exe}
cases=$root/rewatch-ocaml/tests/source_config_cases.tsv

rust=$(realpath "$rust")
ocaml=$(realpath "$ocaml")
mkdir -p "$root/tmp"
work=$(mktemp -d "$root/tmp/rewatch-source-config-XXXXXX")
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/src"
printf 'let value = 1\n' >"$work/src/A.res"

export RESCRIPT_BSC_EXE=${RESCRIPT_BSC_EXE:-$root/_build/default/compiler/bsc/rescript_compiler_main.exe}
export RESCRIPT_RUNTIME=${RESCRIPT_RUNTIME:-$root/packages/@rescript/runtime}

checked=0
while IFS=$'\t' read -r name expected json; do
  if [[ -z "$name" || "$name" == \#* ]]; then
    continue
  fi
  printf '%s\n' "$json" >"$work/rescript.json"
  set +e
  "$rust" compiler-args "$work/src/A.res" >"$work/rust.out" 2>&1
  rust_status=$?
  "$ocaml" compiler-args "$work/src/A.res" >"$work/ocaml.out" 2>&1
  ocaml_status=$?
  set -e

  if [[ "$rust_status" -eq 0 ]]; then
    actual=accept
  else
    actual=reject
  fi
  if [[ "$ocaml_status" -ne "$rust_status" || "$actual" != "$expected" ]]; then
    printf 'Source config case %s: expected %s, Rust=%s, OCaml=%s\n' \
      "$name" "$expected" "$rust_status" "$ocaml_status" >&2
    printf '%s\n' '--- Rust output ---' >&2
    cat "$work/rust.out" >&2
    printf '%s\n' '--- OCaml output ---' >&2
    cat "$work/ocaml.out" >&2
    exit 1
  fi
  checked=$((checked + 1))
done <"$cases"

printf 'Source configuration cases: %d; Rust/OCaml acceptance matched\n' "$checked"
