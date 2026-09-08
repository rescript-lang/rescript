#!/bin/bash
set -eu

root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
rust=${1:-$root/rewatch/target/debug/rescript}
ocaml=${2:-$root/_build/default/rewatch-ocaml/rescript_ocaml.exe}
rust=$(realpath "$rust")
ocaml=$(realpath "$ocaml")
work=$(mktemp -d "${TMPDIR:-/tmp}/rewatch-command-validation-XXXXXX")
trap 'rm -rf "$work"' EXIT

project="$work/project"
mkdir -p "$project/src" "$work/orphan" "$work/empty" "$work/malformed"
mkdir -p "$work/malformed-parent/child/src" "$work/config-directory/rescript.json"
mkdir -p "$work/missing-dependency/src"
mkdir -p "$work/malformed-lock/src" "$work/malformed-lock/lib"
mkdir -p "$work/interface-mismatch/src"
mkdir -p "$work/external-dev-source/src" \
  "$work/external-dev-source/node_modules/dep/src" \
  "$work/external-dev-source/node_modules/dep/test"
mkdir -p "$work/mismatched-dependency/src" \
  "$work/mismatched-dependency/node_modules/dep/src"
mkdir -p "$work/configless-dependency/src" \
  "$work/configless-dependency/node_modules/no-config"
mkdir -p "$work/malformed-dependency/src" \
  "$work/malformed-dependency/node_modules/bad-config"
printf '{"name":"command-validation","sources":["src"]}\n' \
  >"$project/rescript.json"
printf 'let value = 1\n' >"$project/src/A.res"
printf 'not a ReScript source\n' >"$project/src/A.txt"
printf 'let value = 1\n' >"$work/orphan/A.res"
printf '{ invalid json\n' >"$work/malformed/rescript.json"
printf '{ invalid json\n' >"$work/malformed-parent/rescript.json"
printf '{"name":"child","sources":["src"]}\n' \
  >"$work/malformed-parent/child/rescript.json"
printf 'let value = 1\n' >"$work/malformed-parent/child/src/A.res"
printf '{"name":"missing-dependency","sources":["src"],"dependencies":["absent"]}\n' \
  >"$work/missing-dependency/rescript.json"
printf 'let value = 1\n' >"$work/missing-dependency/src/A.res"
printf '{"name":"malformed-lock","sources":["src"]}\n' \
  >"$work/malformed-lock/rescript.json"
printf 'let value = 1\n' >"$work/malformed-lock/src/A.res"
printf '{"name":"interface-mismatch","sources":["src"]}\n' \
  >"$work/interface-mismatch/rescript.json"
printf 'let value = 1\n' >"$work/interface-mismatch/src/lower.res"
printf 'let value: int\n' >"$work/interface-mismatch/src/Lower.resi"
printf '{"name":"external-dev-source","sources":["src"],"dependencies":["dep"]}\n' \
  >"$work/external-dev-source/rescript.json"
printf 'let value = DepPublic.value\n' \
  >"$work/external-dev-source/src/App.res"
printf '{"name":"dep","sources":["src",{"dir":"test","type":"dev"}]}\n' \
  >"$work/external-dev-source/node_modules/dep/rescript.json"
printf 'let value = 1\n' \
  >"$work/external-dev-source/node_modules/dep/src/DepPublic.res"
printf 'this is deliberately invalid ReScript\n' \
  >"$work/external-dev-source/node_modules/dep/test/DevOnly.res"
printf '{"name":"mismatched-dependency","sources":["src"],"dependencies":["dep"]}\n' \
  >"$work/mismatched-dependency/rescript.json"
printf 'let value = Dep.value\n' >"$work/mismatched-dependency/src/A.res"
printf '{"name":"dep","sources":["src"]}\n' \
  >"$work/mismatched-dependency/node_modules/dep/rescript.json"
printf '{"name":"different-name"}\n' \
  >"$work/mismatched-dependency/node_modules/dep/package.json"
printf 'let value = 1\n' \
  >"$work/mismatched-dependency/node_modules/dep/src/Dep.res"
printf '{"name":"configless-dependency","sources":["src"],"dependencies":["no-config"]}\n' \
  >"$work/configless-dependency/rescript.json"
printf 'let value = 1\n' >"$work/configless-dependency/src/A.res"
printf '{"name":"malformed-dependency","sources":["src"],"dependencies":["bad-config"]}\n' \
  >"$work/malformed-dependency/rescript.json"
printf 'let value = 1\n' >"$work/malformed-dependency/src/A.res"
printf '{ invalid json\n' \
  >"$work/malformed-dependency/node_modules/bad-config/rescript.json"

export RESCRIPT_BSC_EXE=${RESCRIPT_BSC_EXE:-$root/_build/default/compiler/bsc/rescript_compiler_main.exe}
export RESCRIPT_RUNTIME=${RESCRIPT_RUNTIME:-$root/packages/@rescript/runtime}

classify() {
  case "$1" in
    0) printf accept ;;
    101) printf panic ;;
    2) printf exit2 ;;
    *) printf reject ;;
  esac
}

checked=0
run_case() {
  name=$1
  rust_expected=$2
  ocaml_expected=$3
  shift 3
  set +e
  "$rust" "$@" >"$work/rust.out" 2>"$work/rust.err"
  rust_status=$?
  "$ocaml" "$@" >"$work/ocaml.out" 2>"$work/ocaml.err"
  ocaml_status=$?
  set -e
  rust_actual=$(classify "$rust_status")
  ocaml_actual=$(classify "$ocaml_status")
  if [ "$rust_actual" != "$rust_expected" ] || \
    [ "$ocaml_actual" != "$ocaml_expected" ]; then
    printf '%s: expected Rust=%s/OCaml=%s, got Rust=%s/OCaml=%s\n' \
      "$name" "$rust_expected" "$ocaml_expected" \
      "$rust_status" "$ocaml_status" >&2
    printf '%s\n' '--- Rust output ---' >&2
    cat "$work/rust.out" "$work/rust.err" >&2
    printf '%s\n' '--- OCaml output ---' >&2
    cat "$work/ocaml.out" "$work/ocaml.err" >&2
    exit 1
  fi
  checked=$((checked + 1))
}

run_case compiler-args-source accept accept compiler-args "$project/src/A.res"
run_case compiler-args-extension accept reject compiler-args "$project/src/A.txt"
run_case compiler-args-missing panic reject compiler-args "$project/src/Missing.res"
run_case compiler-args-no-project panic reject compiler-args "$work/orphan/A.res"

set +e
RESCRIPT_BSC_EXE="$work/missing-bsc" "$rust" build "$project" \
  >"$work/rust.out" 2>"$work/rust.err"
rust_status=$?
RESCRIPT_BSC_EXE="$work/missing-bsc" "$ocaml" build "$project" \
  >"$work/ocaml.out" 2>"$work/ocaml.err"
ocaml_status=$?
set -e
if [ "$(classify "$rust_status")" != panic ] || \
  [ "$(classify "$ocaml_status")" != reject ]; then
  printf 'build-missing-bsc: expected Rust=panic/OCaml=reject, got Rust=%s/OCaml=%s\n' \
    "$rust_status" "$ocaml_status" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.out" "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.out" "$work/ocaml.err" >&2
  exit 1
fi
checked=$((checked + 1))

run_case build-missing-folder reject reject build "$work/missing"
run_case build-existing-folder-without-config reject reject build "$work/empty"
run_case build-malformed-config reject reject build "$work/malformed"
run_case build-malformed-parent reject reject build "$work/malformed-parent/child"
run_case build-config-path-is-directory reject reject build "$work/config-directory"
printf 'not-a-pid' >"$work/malformed-lock/lib/build.lock"
run_case build-malformed-lock reject reject build "$work/malformed-lock"
if [ "$(cat "$work/malformed-lock/lib/build.lock")" != not-a-pid ]; then
  echo "OCaml replaced a malformed build lock with unknown ownership" >&2
  exit 1
fi
printf 'not-a-pid' >"$work/malformed-lock/lib/watch.lock"
run_case watch-malformed-lock reject reject watch "$work/malformed-lock"
if [ "$(cat "$work/malformed-lock/lib/watch.lock")" != not-a-pid ]; then
  echo "OCaml replaced a malformed watch lock with unknown ownership" >&2
  exit 1
fi
run_case build-interface-path-mismatch reject reject build \
  "$work/interface-mismatch"
if ! cmp -s "$work/rust.err" "$work/ocaml.err"; then
  echo "Implementation/interface mismatch diagnostics differ" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.err" >&2
  exit 1
fi
run_case build-excludes-external-dev-source accept accept build \
  "$work/external-dev-source"
run_case build-mismatched-dependency-name panic accept build \
  "$work/mismatched-dependency"
run_case build-missing-dependency exit2 exit2 build "$work/missing-dependency"
run_case build-configless-dependency exit2 exit2 build "$work/configless-dependency"
run_case build-malformed-dependency exit2 exit2 build "$work/malformed-dependency"
run_case clean-missing-dependency exit2 exit2 clean "$work/missing-dependency"
run_case clean-configless-dependency exit2 exit2 clean "$work/configless-dependency"
run_case clean-malformed-dependency exit2 exit2 clean "$work/malformed-dependency"
run_case watch-missing-dependency exit2 exit2 watch "$work/missing-dependency"
if [ -e "$work/missing-dependency/lib/build.lock" ] || \
  [ -e "$work/missing-dependency/lib/watch.lock" ]; then
  echo "OCaml dependency failures left a build or watch lock behind" >&2
  exit 1
fi

set +e
(cd "$project/src" && "$rust" format --check) \
  >"$work/rust.out" 2>"$work/rust.err"
rust_status=$?
(cd "$project/src" && "$ocaml" format --check) \
  >"$work/ocaml.out" 2>"$work/ocaml.err"
ocaml_status=$?
set -e
if [ "$(classify "$rust_status")" != reject ] || \
  [ "$(classify "$ocaml_status")" != reject ]; then
  printf 'format-nested: expected both implementations to reject, got Rust=%s/OCaml=%s\n' \
    "$rust_status" "$ocaml_status" >&2
  exit 1
fi
checked=$((checked + 1))

printf 'Command validation cases: %d; expected outcomes matched\n' "$checked"
