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
mkdir -p "$work/src" "$work/node_modules/dep/lib/ocaml"
printf 'let value = 1\n' >"$work/src/A.res"

export RESCRIPT_BSC_EXE=${RESCRIPT_BSC_EXE:-$root/_build/default/compiler/bsc/rescript_compiler_main.exe}
export RESCRIPT_RUNTIME=${RESCRIPT_RUNTIME:-$root/packages/@rescript/runtime}

checked=0
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
  else
    rust_actual=reject
  fi
  if [[ "$ocaml_status" -eq 0 ]]; then
    ocaml_actual=accept
  else
    ocaml_actual=reject
  fi
  if [[ "$rust_actual" != "$expected" || "$ocaml_actual" != "$expected" ]]; then
    printf 'Config case %s/%s: expected %s, Rust=%s, OCaml=%s\n' \
      "$area" "$name" "$expected" "$rust_status" "$ocaml_status" >&2
    printf '%s\n' '--- Rust output ---' >&2
    cat "$work/rust.out" >&2
    cat "$work/rust.err" >&2
    printf '%s\n' '--- OCaml output ---' >&2
    cat "$work/ocaml.out" >&2
    cat "$work/ocaml.err" >&2
    exit 1
  fi
  if [[ "$expected" == accept ]] &&
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

printf 'Configuration cases: %d; Rust/OCaml acceptance and arguments matched\n' \
  "$checked"
