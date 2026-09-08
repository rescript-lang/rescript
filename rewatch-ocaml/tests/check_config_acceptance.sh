#!/bin/bash
set -eu

root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
rust=${1:-$root/rewatch/target/debug/rescript}
ocaml=${2:-$root/_build/default/rewatch-ocaml/rescript_ocaml.exe}
cases=$root/rewatch-ocaml/tests/config_acceptance_cases.tsv

rust=$(realpath "$rust")
ocaml=$(realpath "$ocaml")
mkdir -p "$root/tmp"
work=$(mktemp -d "$root/tmp/rewatch-config-acceptance-XXXXXX")
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/src" "$work/node_modules/dep/lib/ocaml" "$work/node_modules/ppx"
printf 'let value = 1\n' >"$work/src/A.res"

export RESCRIPT_BSC_EXE=${RESCRIPT_BSC_EXE:-$root/_build/default/compiler/bsc/rescript_compiler_main.exe}
export RESCRIPT_RUNTIME=${RESCRIPT_RUNTIME:-$root/packages/@rescript/runtime}

checked=0
divergences=0
while IFS=$'\t' read -r area name expected json; do
  if [[ -z "$area" || "$area" == \#* ]]; then
    continue
  fi
  printf '%s\n' "$json" >"$work/rescript.json"
  set +e
  "$rust" compiler-args "$work/src/A.res" >"$work/rust.out" 2>"$work/rust.err"
  rust_status=$?
  "$ocaml" compiler-args "$work/src/A.res" >"$work/ocaml.out" 2>"$work/ocaml.err"
  ocaml_status=$?
  set -e

  if [[ "$rust_status" -eq 0 ]]; then
    rust_actual=accept
  elif [[ "$rust_status" -eq 101 ]]; then
    rust_actual=panic
  else
    rust_actual=reject
  fi
  if [[ "$ocaml_status" -eq 0 ]]; then
    ocaml_actual=accept
  else
    ocaml_actual=reject
  fi
  rust_expected=${expected%%/*}
  if [[ "$expected" == */* ]]; then
    ocaml_expected=${expected#*/}
    compare_arguments=false
    divergences=$((divergences + 1))
  else
    ocaml_expected=$expected
    compare_arguments=true
  fi
  if [[ "$rust_actual" != "$rust_expected" || "$ocaml_actual" != "$ocaml_expected" ]]; then
    printf 'Config case %s/%s: expected Rust=%s/OCaml=%s, got Rust=%s/OCaml=%s\n' \
      "$area" "$name" "$rust_expected" "$ocaml_expected" \
      "$rust_status" "$ocaml_status" >&2
    printf '%s\n' '--- Rust output ---' >&2
    cat "$work/rust.out" >&2
    cat "$work/rust.err" >&2
    printf '%s\n' '--- OCaml output ---' >&2
    cat "$work/ocaml.out" >&2
    cat "$work/ocaml.err" >&2
    exit 1
  fi
  if [[ "$compare_arguments" == true && "$rust_expected" == accept ]] &&
    ! node -e '
      const fs = require("fs");
      const assert = require("assert");
      assert.deepStrictEqual(
        JSON.parse(fs.readFileSync(process.argv[1], "utf8")),
        JSON.parse(fs.readFileSync(process.argv[2], "utf8")),
      );
    ' "$work/rust.out" "$work/ocaml.out"; then
    printf 'Config case %s/%s produced different compiler arguments\n' \
      "$area" "$name" >&2
    diff -u "$work/rust.out" "$work/ocaml.out" >&2 || true
    exit 1
  fi
  checked=$((checked + 1))
done <"$cases"

printf 'Configuration cases: %d (%d documented divergences); Rust/OCaml expectations and parity arguments matched\n' \
  "$checked" "$divergences"
