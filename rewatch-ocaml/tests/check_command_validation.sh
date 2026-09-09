#!/bin/bash
set -eu

root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
rust=${1:-$root/rewatch/target/debug/rescript}
ocaml=${2:-$root/_build/default/rewatch-ocaml/rescript_ocaml.exe}
rust=$(realpath "$rust")
ocaml=$(realpath "$ocaml")
work=$(mktemp -d "${TMPDIR:-/tmp}/rewatch-command-validation-XXXXXX")
background_pids=""
cleanup() {
  for pid in $background_pids; do
    kill -TERM "$pid" 2>/dev/null || true
    wait "$pid" 2>/dev/null || true
  done
  rm -rf "$work"
}
trap cleanup EXIT

project="$work/project"
mkdir -p "$project/src" "$work/orphan" "$work/empty" "$work/malformed"
mkdir -p "$work/malformed-parent/child/src" "$work/config-directory/rescript.json"
mkdir -p "$work/missing-dependency/src"
mkdir -p "$work/malformed-lock/src" "$work/malformed-lock/lib"
mkdir -p "$work/interface-mismatch/src"
mkdir -p "$work/exotic-module-rust/src" "$work/exotic-module-ocaml/src"
mkdir -p "$work/filter-basename-rust/src/nested" \
  "$work/filter-basename-ocaml/src/nested"
mkdir -p "$work/external-dev-source/src" \
  "$work/external-dev-source/node_modules/dep/src" \
  "$work/external-dev-source/node_modules/dep/test"
mkdir -p "$work/external-dev-permission/src" \
  "$work/external-dev-permission/node_modules/a" \
  "$work/external-dev-permission/node_modules/b"
mkdir -p "$work/missing-source-folder/src" \
  "$work/missing-source-folder/node_modules/dep"
mkdir -p "$work/dependency-without-sources/src" \
  "$work/dependency-without-sources/node_modules/dep"
mkdir -p "$work/default-feature-cycle/src"
mkdir -p "$work/format-feature-cycle/src" \
  "$work/format-feature-cycle/node_modules/dep/src"
mkdir -p "$work/format-source-selection/src" \
  "$work/format-source-selection/node_modules/installed" \
  "$work/format-source-selection/packages/local/base" \
  "$work/format-source-selection/packages/local/native" \
  "$work/format-source-selection/packages/local/other"
mkdir -p "$work/package-name-mismatch/src" "$work/malformed-package-json/src"
mkdir -p "$work/failed-js-post-build/src"
mkdir -p "$work/mismatched-dependency/src" \
  "$work/mismatched-dependency/node_modules/dep/src"
mkdir -p "$work/configless-dependency/src" \
  "$work/configless-dependency/node_modules/no-config"
mkdir -p "$work/malformed-dependency/src" \
  "$work/malformed-dependency/node_modules/bad-config"
mkdir -p "$work/duplicate-dependency/src" \
  "$work/duplicate-dependency/node_modules/a/src" \
  "$work/duplicate-dependency/node_modules/shared/src" \
  "$work/duplicate-dependency/node_modules/a/node_modules/shared/src"
mkdir -p "$work/publication-race-rust/src" \
  "$work/publication-race-ocaml/src"
mkdir -p "$work/ast-race-rust/src" "$work/ast-race-ocaml/src"
mkdir -p "$work/watch-config-rust/src" "$work/watch-config-ocaml/src"
mkdir -p "$work/watch-filter-rust/src" "$work/watch-filter-ocaml/src"
printf '{"name":"command-validation","sources":["src"]}\n' \
  >"$project/rescript.json"
printf 'let value = 1\n' >"$project/src/A.res"
printf 'not a ReScript source\n' >"$project/src/A.txt"
mkdir -p "$work/redirected-parse-fixture/src"
printf '{"name":"parse-output","sources":["src"]}\n' \
  >"$work/redirected-parse-fixture/rescript.json"
printf 'let value =\n' >"$work/redirected-parse-fixture/src/A.res"
printf 'process.stderr.write("hook failed\\n"); process.exit(7)\n' \
  >"$work/failing-after-build.js"
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
for implementation in rust ocaml; do
  printf '{"name":"exotic-module","namespace":"Ns","sources":["src"]}\n' \
    >"$work/exotic-module-$implementation/rescript.json"
  printf 'let value = 1\n' \
    >"$work/exotic-module-$implementation/src/Main.res"
  printf 'let value = 2\n' \
    >"$work/exotic-module-$implementation/src/foo-bar.res"
  printf '{"name":"filter-basename","sources":[{"dir":"src","subdirs":true}]}\n' \
    >"$work/filter-basename-$implementation/rescript.json"
  printf 'let value = 1\n' \
    >"$work/filter-basename-$implementation/src/nested/A.res"
done
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
printf '{"name":"root","sources":["src"],"dependencies":["a","b"]}\n' \
  >"$work/external-dev-permission/rescript.json"
printf '{"name":"a","sources":[],"dev-dependencies":["b"]}\n' \
  >"$work/external-dev-permission/node_modules/a/rescript.json"
printf '{"name":"b","sources":[],"allowed-dependents":["root"]}\n' \
  >"$work/external-dev-permission/node_modules/b/rescript.json"
printf '{"name":"missing-source-folder","sources":["src"],"dependencies":["dep"]}\n' \
  >"$work/missing-source-folder/rescript.json"
printf 'let value = 1\n' >"$work/missing-source-folder/src/App.res"
printf '{"name":"dep","sources":["missing"]}\n' \
  >"$work/missing-source-folder/node_modules/dep/rescript.json"
printf '{"name":"dependency-without-sources","sources":["src"],"dependencies":["dep"]}\n' \
  >"$work/dependency-without-sources/rescript.json"
printf 'let value = 1\n' >"$work/dependency-without-sources/src/App.res"
printf '{"name":"dep"}\n' \
  >"$work/dependency-without-sources/node_modules/dep/rescript.json"
printf '{"name":"dep"}\n' \
  >"$work/dependency-without-sources/node_modules/dep/package.json"
printf '{"name":"default-feature-cycle","sources":["src"],"features":{"a":["b"],"b":["a"]}}\n' \
  >"$work/default-feature-cycle/rescript.json"
printf 'let value = 1\n' >"$work/default-feature-cycle/src/App.res"
printf '{"name":"format-feature-cycle","sources":["src"],"dependencies":[{"name":"dep","features":["a"]}]}\n' \
  >"$work/format-feature-cycle/rescript.json"
printf 'let value = 1\n' >"$work/format-feature-cycle/src/App.res"
printf '{"name":"dep","sources":["src"],"features":{"a":["b"],"b":["a"]}}\n' \
  >"$work/format-feature-cycle/node_modules/dep/rescript.json"
printf '{"name":"dep"}\n' \
  >"$work/format-feature-cycle/node_modules/dep/package.json"
printf 'let value = 1\n' \
  >"$work/format-feature-cycle/node_modules/dep/src/Dep.res"
printf '{"name":"format-source-selection","sources":["src"],"dependencies":["installed",{"name":"local","features":["native"]}]}\n' \
  >"$work/format-source-selection/rescript.json"
printf '{"name":"installed","sources":["missing"]}\n' \
  >"$work/format-source-selection/node_modules/installed/rescript.json"
printf '{"name":"local","sources":["base",{"dir":"native","feature":"native"},{"dir":"other","feature":"other"}]}\n' \
  >"$work/format-source-selection/packages/local/rescript.json"
printf 'let value=1\n' \
  >"$work/format-source-selection/packages/local/base/Base.res"
printf 'let value=2\n' \
  >"$work/format-source-selection/packages/local/native/Native.res"
printf 'let value=3\n' \
  >"$work/format-source-selection/packages/local/other/Other.res"
ln -s ../packages/local "$work/format-source-selection/node_modules/local"
printf '{"name":"config-name","sources":["src"]}\n' \
  >"$work/package-name-mismatch/rescript.json"
printf '{"name":"package-name"}\n' >"$work/package-name-mismatch/package.json"
printf 'let value = 1\n' >"$work/package-name-mismatch/src/A.res"
printf '{"name":"malformed-package-json","sources":["src"]}\n' \
  >"$work/malformed-package-json/rescript.json"
printf '{invalid\n' >"$work/malformed-package-json/package.json"
printf 'let value = 1\n' >"$work/malformed-package-json/src/A.res"
printf '{"name":"failed-js-post-build","sources":["src"],"js-post-build":{"cmd":"exit 7"}}\n' \
  >"$work/failed-js-post-build/rescript.json"
printf 'let value = 1\n' >"$work/failed-js-post-build/src/A.res"
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
printf '{"name":"duplicate-dependency","sources":["src"],"dependencies":["a","shared"]}\n' \
  >"$work/duplicate-dependency/rescript.json"
printf 'let value = Shared.value + A.value\n' \
  >"$work/duplicate-dependency/src/Main.res"
printf '{"name":"a","sources":["src"],"dependencies":["shared"]}\n' \
  >"$work/duplicate-dependency/node_modules/a/rescript.json"
printf 'let value = Shared.value\n' \
  >"$work/duplicate-dependency/node_modules/a/src/A.res"
printf '{"name":"shared","sources":["src"]}\n' \
  >"$work/duplicate-dependency/node_modules/shared/rescript.json"
printf 'let value = 1\n' \
  >"$work/duplicate-dependency/node_modules/shared/src/Shared.res"
printf '{"name":"shared","sources":["src"]}\n' \
  >"$work/duplicate-dependency/node_modules/a/node_modules/shared/rescript.json"
printf 'let value = 2\n' \
  >"$work/duplicate-dependency/node_modules/a/node_modules/shared/src/Shared.res"
printf '{"name":"publication-race","sources":["src"]}\n' \
  >"$work/publication-race-rust/rescript.json"
cp "$work/publication-race-rust/rescript.json" \
  "$work/publication-race-ocaml/rescript.json"
printf 'let value = 1\n' >"$work/publication-race-rust/src/A.res"
cp "$work/publication-race-rust/src/A.res" \
  "$work/publication-race-ocaml/src/A.res"
printf '{"name":"ast-race","sources":["src"]}\n' \
  >"$work/ast-race-rust/rescript.json"
cp "$work/ast-race-rust/rescript.json" "$work/ast-race-ocaml/rescript.json"
printf 'let value = 1\n' >"$work/ast-race-rust/src/A.res"
cp "$work/ast-race-rust/src/A.res" "$work/ast-race-ocaml/src/A.res"
printf '{"name":"watch-config","sources":["src"]}\n' \
  >"$work/watch-config-rust/rescript.json"
cp "$work/watch-config-rust/rescript.json" \
  "$work/watch-config-ocaml/rescript.json"
printf 'let value = 1\n' >"$work/watch-config-rust/src/A.res"
cp "$work/watch-config-rust/src/A.res" "$work/watch-config-ocaml/src/A.res"
for implementation in rust ocaml; do
  printf '{"name":"watch-filter","sources":["src"]}\n' \
    >"$work/watch-filter-$implementation/rescript.json"
  printf 'let value = 1\n' \
    >"$work/watch-filter-$implementation/src/Include.res"
  printf 'let value = 10\n' \
    >"$work/watch-filter-$implementation/src/Exclude.res"
done
printf 'require("fs").appendFileSync(process.env.REWATCH_WATCH_FILTER_MARKER, "done\\n")\n' \
  >"$work/watch-filter-marker.js"

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

run_cwd_case() {
  name=$1
  rust_expected=$2
  ocaml_expected=$3
  cwd=$4
  shift 4
  set +e
  (cd "$cwd" && "$rust" "$@") >"$work/rust.out" 2>"$work/rust.err"
  rust_status=$?
  (cd "$cwd" && "$ocaml" "$@") >"$work/ocaml.out" 2>"$work/ocaml.err"
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

require_same_output() {
  name=$1
  if ! cmp -s "$work/rust.out" "$work/ocaml.out" || \
    ! cmp -s "$work/rust.err" "$work/ocaml.err"; then
    echo "$name: Rust and OCaml output differ" >&2
    printf '%s\n' '--- Rust output ---' >&2
    cat "$work/rust.out" "$work/rust.err" >&2
    printf '%s\n' '--- OCaml output ---' >&2
    cat "$work/ocaml.out" "$work/ocaml.err" >&2
    exit 1
  fi
}

require_both_errors_contain() {
  name=$1
  fragment=$2
  if ! grep -F "$fragment" "$work/rust.err" >/dev/null || \
    ! grep -F "$fragment" "$work/ocaml.err" >/dev/null; then
    printf '%s: expected both errors to contain %s\n' "$name" "$fragment" >&2
    printf '%s\n' '--- Rust output ---' >&2
    cat "$work/rust.out" "$work/rust.err" >&2
    printf '%s\n' '--- OCaml output ---' >&2
    cat "$work/ocaml.out" "$work/ocaml.err" >&2
    exit 1
  fi
}

run_missing_bsc_case() {
  name=$1
  shift
  set +e
  RESCRIPT_BSC_EXE="$work/missing-bsc" "$rust" "$@" \
    >"$work/rust.out" 2>"$work/rust.err"
  rust_status=$?
  RESCRIPT_BSC_EXE="$work/missing-bsc" "$ocaml" "$@" \
    >"$work/ocaml.out" 2>"$work/ocaml.err"
  ocaml_status=$?
  set -e
  if [ "$(classify "$rust_status")" != panic ] || \
    [ "$(classify "$ocaml_status")" != reject ]; then
    printf '%s: expected Rust=panic/OCaml=reject, got Rust=%s/OCaml=%s\n' \
      "$name" "$rust_status" "$ocaml_status" >&2
    printf '%s\n' '--- Rust output ---' >&2
    cat "$work/rust.out" "$work/rust.err" >&2
    printf '%s\n' '--- OCaml output ---' >&2
    cat "$work/ocaml.out" "$work/ocaml.err" >&2
    exit 1
  fi
  if ! grep -F 'RESCRIPT_BSC_EXE points to missing path' \
    "$work/ocaml.err" >/dev/null; then
    echo "$name: OCaml did not report the stale compiler path" >&2
    cat "$work/ocaml.out" "$work/ocaml.err" >&2
    exit 1
  fi
  checked=$((checked + 1))
}

run_redirected_build_case() {
  name=$1
  expected_status=$2
  fixture=$3
  rust_project="$work/$name-rust"
  ocaml_project="$work/$name-ocaml"
  cp -R "$fixture" "$rust_project"
  cp -R "$fixture" "$ocaml_project"
  set +e
  "$rust" build "$rust_project" >"$work/rust.out" 2>"$work/rust.err"
  rust_status=$?
  "$ocaml" build "$ocaml_project" >"$work/ocaml.out" 2>"$work/ocaml.err"
  ocaml_status=$?
  set -e
  sed "s|$rust_project|<ROOT>|g" "$work/rust.out" >"$work/rust.out.norm"
  sed "s|$rust_project|<ROOT>|g" "$work/rust.err" >"$work/rust.err.norm"
  sed "s|$ocaml_project|<ROOT>|g" "$work/ocaml.out" >"$work/ocaml.out.norm"
  sed "s|$ocaml_project|<ROOT>|g" "$work/ocaml.err" >"$work/ocaml.err.norm"
  if [ "$rust_status" -ne "$expected_status" ] || \
    [ "$ocaml_status" -ne "$expected_status" ] || \
    ! cmp -s "$work/rust.out.norm" "$work/ocaml.out.norm" || \
    ! cmp -s "$work/rust.err.norm" "$work/ocaml.err.norm"; then
    echo "$name: redirected build output differs" >&2
    printf '%s\n' '--- Rust stdout ---' >&2
    cat "$work/rust.out.norm" >&2
    printf '%s\n' '--- OCaml stdout ---' >&2
    cat "$work/ocaml.out.norm" >&2
    printf '%s\n' '--- Rust stderr ---' >&2
    cat "$work/rust.err.norm" >&2
    printf '%s\n' '--- OCaml stderr ---' >&2
    cat "$work/ocaml.err.norm" >&2
    exit 1
  fi
  checked=$((checked + 1))
}

wait_for_file() {
  path=$1
  attempts=0
  while [ "$attempts" -lt 150 ] && [ ! -f "$path" ]; do
    attempts=$((attempts + 1))
    sleep 0.1
  done
  [ -f "$path" ]
}

wait_for_text() {
  path=$1
  pattern=$2
  attempts=0
  while [ "$attempts" -lt 150 ] && \
    ! grep -F "$pattern" "$path" >/dev/null 2>&1; do
    attempts=$((attempts + 1))
    sleep 0.1
  done
  grep -F "$pattern" "$path" >/dev/null 2>&1
}

wait_for_exit() {
  pid=$1
  attempts=0
  while [ "$attempts" -lt 150 ] && kill -0 "$pid" 2>/dev/null; do
    attempts=$((attempts + 1))
    sleep 0.1
  done
  ! kill -0 "$pid" 2>/dev/null
}

wait_for_line_count() {
  path=$1
  expected=$2
  attempts=0
  while [ "$attempts" -lt 150 ]; do
    actual=0
    if [ -f "$path" ]; then
      actual=$(wc -l <"$path" | tr -d ' ')
    fi
    if [ "$actual" -ge "$expected" ]; then
      return 0
    fi
    attempts=$((attempts + 1))
    sleep 0.1
  done
  return 1
}

run_redirected_build_case redirected-compile-error 1 \
  "$root/rewatch-ocaml/tests/failure"
run_redirected_build_case redirected-parse-error 1 \
  "$work/redirected-parse-fixture"
run_redirected_build_case redirected-warning 0 \
  "$root/rewatch-ocaml/tests/warning-replay"

run_case compiler-args-source accept accept compiler-args "$project/src/A.res"
run_case compiler-args-extension accept reject compiler-args "$project/src/A.txt"
run_case compiler-args-missing panic reject compiler-args "$project/src/Missing.res"
run_case compiler-args-no-project panic reject compiler-args "$work/orphan/A.res"

run_missing_bsc_case build-missing-bsc build "$project"
run_missing_bsc_case format-missing-bsc format "$project/src/A.res"
run_case format-missing-file reject reject format "$project/src/Missing.res"
require_same_output format-missing-file
run_case format-directory reject reject format "$project/src"
require_same_output format-directory
run_case format-unsupported-extension reject reject format "$project/src/A.txt"
require_same_output format-unsupported-extension
run_cwd_case format-no-config reject reject "$work/empty" format
require_both_errors_contain format-no-config \
  "Could not read rescript.json at $work/empty:"
require_both_errors_contain format-no-config "$work/empty/bsconfig.json"
run_cwd_case format-malformed-config reject reject "$work/malformed" format
require_both_errors_contain format-malformed-config \
  "Could not read rescript.json at $work/malformed:"
if ! grep -F 'Failed to parse rescript.json' "$work/rust.err" >/dev/null || \
  ! grep -F 'invalid JSON' "$work/ocaml.err" >/dev/null; then
  echo "format-malformed-config: JSON parser context was lost" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.out" "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.out" "$work/ocaml.err" >&2
  exit 1
fi
run_cwd_case format-config-directory reject reject "$work/config-directory" format
require_both_errors_contain format-config-directory \
  "$work/config-directory/rescript.json"
require_both_errors_contain format-config-directory 'Is a directory'

run_case build-missing-folder reject reject build "$work/missing"
run_case build-existing-folder-without-config reject reject build "$work/empty"
run_case build-malformed-config reject reject build "$work/malformed"
run_case build-malformed-parent reject reject build "$work/malformed-parent/child"
run_case build-config-path-is-directory reject reject build "$work/config-directory"
run_case after-build-nonzero-is-not-ignored accept reject build --after-build \
  "node $work/failing-after-build.js" "$project"
if ! grep -F 'hook failed' "$work/rust.err" >/dev/null || \
  ! grep -F -- '--after-build command failed with exit code 7' \
    "$work/ocaml.err" >/dev/null || \
  ! grep -F 'hook failed' "$work/ocaml.err" >/dev/null; then
  echo "Nonzero --after-build handling differs from its recorded outcomes" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.out" "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.out" "$work/ocaml.err" >&2
  exit 1
fi
run_case after-build-empty panic reject build --after-build '' "$project"
if ! grep -F -- '--after-build command cannot be empty' \
  "$work/ocaml.err" >/dev/null; then
  echo "Empty --after-build did not produce a contextual OCaml error" >&2
  cat "$work/ocaml.out" "$work/ocaml.err" >&2
  exit 1
fi
run_case after-build-missing-program panic reject build --after-build \
  rewatch-command-that-does-not-exist "$project"
if ! grep -F 'Could not run --after-build command' \
    "$work/ocaml.err" >/dev/null || \
  ! grep -F 'rewatch-command-that-does-not-exist' \
    "$work/ocaml.err" >/dev/null; then
  echo "Missing --after-build program did not produce a contextual OCaml error" >&2
  cat "$work/ocaml.out" "$work/ocaml.err" >&2
  exit 1
fi
run_case js-post-build-nonzero reject reject build "$work/failed-js-post-build"
js_post_build_error="js-post-build command failed for $work/failed-js-post-build/src/A.js"
if ! grep -F "$js_post_build_error" "$work/rust.err" >/dev/null || \
  ! grep -F "$js_post_build_error" "$work/ocaml.err" >/dev/null; then
  echo "js-post-build failure diagnostics differ" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.out" "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.out" "$work/ocaml.err" >&2
  exit 1
fi
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
set +e
"$rust" build "$work/exotic-module-rust" \
  >"$work/rust.out" 2>"$work/rust.err"
rust_status=$?
"$ocaml" build "$work/exotic-module-ocaml" \
  >"$work/ocaml.out" 2>"$work/ocaml.err"
ocaml_status=$?
set -e
rust_mlmap="$work/exotic-module-rust/lib/bs/Ns.mlmap"
ocaml_mlmap="$work/exotic-module-ocaml/lib/bs/Ns.mlmap"
if [ "$(classify "$rust_status")" != accept ] || \
  [ "$(classify "$ocaml_status")" != accept ] || \
  ! cmp -s "$rust_mlmap" "$ocaml_mlmap" || \
  ! grep -Fx Main "$ocaml_mlmap" >/dev/null || \
  grep -F 'Foo-bar' "$ocaml_mlmap" >/dev/null; then
  printf 'namespace-exotic-module: expected matching successful builds, got Rust=%s/OCaml=%s\n' \
    "$rust_status" "$ocaml_status" >&2
  printf '%s\n' '--- Rust output / namespace map ---' >&2
  cat "$work/rust.out" "$work/rust.err" "$rust_mlmap" >&2
  printf '%s\n' '--- OCaml output / namespace map ---' >&2
  cat "$work/ocaml.out" "$work/ocaml.err" "$ocaml_mlmap" >&2
  exit 1
fi
checked=$((checked + 1))
set +e
"$rust" build --filter nested "$work/filter-basename-rust" \
  >"$work/rust.out" 2>"$work/rust.err"
rust_status=$?
"$ocaml" build --filter nested "$work/filter-basename-ocaml" \
  >"$work/ocaml.out" 2>"$work/ocaml.err"
ocaml_status=$?
set -e
if [ "$(classify "$rust_status")" != accept ] || \
  [ "$(classify "$ocaml_status")" != accept ] || \
  [ -e "$work/filter-basename-rust/src/nested/A.js" ] || \
  [ -e "$work/filter-basename-ocaml/src/nested/A.js" ]; then
  echo "Source filters did not consistently ignore directory-only matches" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.out" "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.out" "$work/ocaml.err" >&2
  exit 1
fi
checked=$((checked + 1))
set +e
"$rust" build --filter 'A\.res$' "$work/filter-basename-rust" \
  >"$work/rust.out" 2>"$work/rust.err"
rust_status=$?
"$ocaml" build --filter 'A\.res$' "$work/filter-basename-ocaml" \
  >"$work/ocaml.out" 2>"$work/ocaml.err"
ocaml_status=$?
set -e
if [ "$(classify "$rust_status")" != accept ] || \
  [ "$(classify "$ocaml_status")" != accept ] || \
  [ ! -e "$work/filter-basename-rust/src/nested/A.js" ] || \
  [ ! -e "$work/filter-basename-ocaml/src/nested/A.js" ]; then
  echo "Source filters did not consistently include a basename match" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.out" "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.out" "$work/ocaml.err" >&2
  exit 1
fi
checked=$((checked + 1))
run_case build-excludes-external-dev-source accept accept build \
  "$work/external-dev-source"
run_case build-ignores-dormant-external-dev-permission reject accept build \
  "$work/external-dev-permission"
if ! grep -F 'a has the following unallowed dependencies' \
    "$work/rust.err" >/dev/null; then
  echo "Rust dormant external dev-dependency rejection was not reproduced" >&2
  cat "$work/rust.out" "$work/rust.err" >&2
  exit 1
fi
run_case build-missing-source-folder accept accept build \
  "$work/missing-source-folder"
if ! cmp -s "$work/rust.err" "$work/ocaml.err"; then
  echo "Missing source-folder diagnostics differ" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.err" >&2
  exit 1
fi
run_case build-dependency-without-sources accept accept build \
  "$work/dependency-without-sources"
if ! cmp -s "$work/rust.err" "$work/ocaml.err"; then
  echo "Dependency-without-sources diagnostics differ" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.err" >&2
  exit 1
fi
run_case build-default-feature-cycle accept accept build \
  "$work/default-feature-cycle"
run_cwd_case format-requested-feature-cycle reject reject \
  "$work/format-feature-cycle" format
if ! cmp -s "$work/rust.err" "$work/ocaml.err"; then
  echo "Requested format feature-cycle diagnostics differ" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.out" "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.out" "$work/ocaml.err" >&2
  exit 1
fi
run_cwd_case format-scans-graph-with-effective-features reject reject \
  "$work/format-source-selection" format --check
missing_folder='Could not read folder: "missing". Specified in dependency: installed'
base_file="[format check] $work/format-source-selection/packages/local/base/Base.res"
native_file="[format check] $work/format-source-selection/packages/local/native/Native.res"
other_file="$work/format-source-selection/packages/local/other/Other.res"
if ! grep -F "$missing_folder" "$work/rust.err" >/dev/null || \
  ! grep -F "$missing_folder" "$work/ocaml.err" >/dev/null || \
  ! grep -F "$base_file" "$work/rust.err" >/dev/null || \
  ! grep -F "$base_file" "$work/ocaml.err" >/dev/null || \
  ! grep -F "$native_file" "$work/rust.err" >/dev/null || \
  ! grep -F "$native_file" "$work/ocaml.err" >/dev/null || \
  grep -F "$other_file" "$work/rust.err" >/dev/null || \
  grep -F "$other_file" "$work/ocaml.err" >/dev/null || \
  ! grep -F 'The 2 files listed above need formatting' "$work/rust.err" >/dev/null || \
  ! grep -F 'The 2 files listed above need formatting' "$work/ocaml.err" >/dev/null; then
  echo "Implicit format package scanning or feature selection differs" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.out" "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.out" "$work/ocaml.err" >&2
  exit 1
fi
run_case clean-dependency-without-sources accept accept clean \
  "$work/dependency-without-sources"
if ! cmp -s "$work/rust.err" "$work/ocaml.err"; then
  echo "Clean dependency-without-sources diagnostics differ" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.err" >&2
  exit 1
fi
set +e
"$rust" clean "$project" >"$work/rust.out" 2>"$work/rust.err"
rust_status=$?
"$ocaml" clean "$project" >"$work/ocaml.out" 2>"$work/ocaml.err"
ocaml_status=$?
set -e
if [ "$rust_status" -ne 0 ] || [ "$ocaml_status" -ne 0 ] || \
  ! cmp -s "$work/rust.out" "$work/ocaml.out" || \
  ! cmp -s "$work/rust.err" "$work/ocaml.err" || \
  ! grep -Fx 'Cleaning command-validation' "$work/ocaml.out" >/dev/null; then
  echo "Redirected clean progress differs" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.out" "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.out" "$work/ocaml.err" >&2
  exit 1
fi
checked=$((checked + 1))
set +e
"$rust" -q clean "$project" >"$work/rust.out" 2>"$work/rust.err"
rust_status=$?
"$ocaml" -q clean "$project" >"$work/ocaml.out" 2>"$work/ocaml.err"
ocaml_status=$?
set -e
if [ "$rust_status" -ne 0 ] || [ "$ocaml_status" -ne 0 ] || \
  [ -s "$work/rust.out" ] || [ -s "$work/rust.err" ] || \
  [ -s "$work/ocaml.out" ] || [ -s "$work/ocaml.err" ]; then
  echo "Quiet redirected clean emitted output" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.out" "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.out" "$work/ocaml.err" >&2
  exit 1
fi
checked=$((checked + 1))
run_cwd_case format-dependency-without-sources accept accept \
  "$work/dependency-without-sources" format
if ! cmp -s "$work/rust.err" "$work/ocaml.err"; then
  echo "Format dependency-without-sources diagnostics differ" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.out" "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.out" "$work/ocaml.err" >&2
  exit 1
fi
run_case build-package-name-mismatch accept accept build \
  "$work/package-name-mismatch"
if ! cmp -s "$work/rust.err" "$work/ocaml.err"; then
  echo "Package-name mismatch diagnostics differ" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.err" >&2
  exit 1
fi
run_case build-malformed-package-json reject reject build \
  "$work/malformed-package-json"
run_case build-mismatched-dependency-name panic accept build \
  "$work/mismatched-dependency"
run_case build-missing-dependency exit2 exit2 build "$work/missing-dependency"
run_case build-configless-dependency exit2 exit2 build "$work/configless-dependency"
run_case build-malformed-dependency exit2 exit2 build "$work/malformed-dependency"
run_case build-duplicate-dependency accept accept build "$work/duplicate-dependency"
duplicate_warning='Duplicated package: shared ./node_modules/shared (chosen) vs ./node_modules/a/node_modules/shared in ./node_modules/a'
if ! grep -F "$duplicate_warning" "$work/rust.err" >/dev/null || \
  ! grep -F "$duplicate_warning" "$work/ocaml.err" >/dev/null; then
  echo "Duplicate dependency warning was not emitted by both implementations" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.out" "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.out" "$work/ocaml.err" >&2
  exit 1
fi
run_cwd_case format-duplicate-dependency accept accept \
  "$work/duplicate-dependency" format
if ! grep -F "$duplicate_warning" "$work/rust.err" >/dev/null || \
  ! grep -F "$duplicate_warning" "$work/ocaml.err" >/dev/null; then
  echo "Format duplicate dependency warning was not emitted by both implementations" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.out" "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.out" "$work/ocaml.err" >&2
  exit 1
fi

set +e
REWATCH_REAL_BSC="$RESCRIPT_BSC_EXE" \
REWATCH_SOURCE_TO_DELETE="$work/publication-race-rust/src/A.res" \
REWATCH_SOURCE_DELETED="$work/publication-race-rust/source-deleted" \
RESCRIPT_BSC_EXE="$root/rewatch-ocaml/tests/delete-source-bsc.sh" \
  "$rust" build "$work/publication-race-rust" \
  >"$work/rust.out" 2>"$work/rust.err" &
rust_pid=$!
attempts=0
while kill -0 "$rust_pid" 2>/dev/null && [ "$attempts" -lt 150 ]; do
  attempts=$((attempts + 1))
  sleep 0.1
done
if kill -0 "$rust_pid" 2>/dev/null; then
  kill -TERM "$rust_pid" 2>/dev/null
  wait "$rust_pid" 2>/dev/null
  rust_status=124
else
  wait "$rust_pid"
  rust_status=$?
fi
REWATCH_REAL_BSC="$RESCRIPT_BSC_EXE" \
REWATCH_SOURCE_TO_DELETE="$work/publication-race-ocaml/src/A.res" \
REWATCH_SOURCE_DELETED="$work/publication-race-ocaml/source-deleted" \
RESCRIPT_BSC_EXE="$root/rewatch-ocaml/tests/delete-source-bsc.sh" \
  "$ocaml" build "$work/publication-race-ocaml" \
  >"$work/ocaml.out" 2>"$work/ocaml.err"
ocaml_status=$?
set -e
if [ "$rust_status" -ne 124 ] || \
  ! grep -F "copying source file failed" "$work/rust.err" >/dev/null || \
  [ "$(classify "$ocaml_status")" != reject ] || \
  ! grep -F "A.res" "$work/ocaml.err" >/dev/null; then
  printf 'build-source-disappears-during-publication: expected Rust=worker-panic/timeout and OCaml=path-bearing rejection, got Rust=%s/OCaml=%s\n' \
    "$rust_status" "$ocaml_status" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.out" "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.out" "$work/ocaml.err" >&2
  exit 1
fi
checked=$((checked + 1))

set +e
REWATCH_REAL_BSC="$RESCRIPT_BSC_EXE" \
REWATCH_AST_DELETED="$work/ast-race-rust/ast-deleted" \
RESCRIPT_BSC_EXE="$root/rewatch-ocaml/tests/delete-ast-bsc.sh" \
  "$rust" build "$work/ast-race-rust" \
  >"$work/rust.out" 2>"$work/rust.err"
rust_status=$?
REWATCH_REAL_BSC="$RESCRIPT_BSC_EXE" \
REWATCH_AST_DELETED="$work/ast-race-ocaml/ast-deleted" \
RESCRIPT_BSC_EXE="$root/rewatch-ocaml/tests/delete-ast-bsc.sh" \
  "$ocaml" build "$work/ast-race-ocaml" \
  >"$work/ocaml.out" 2>"$work/ocaml.err"
ocaml_status=$?
set -e
if [ "$rust_status" -ne 101 ] || \
  ! grep -F "Could not read file" "$work/rust.err" >/dev/null || \
  [ "$(classify "$ocaml_status")" != reject ] || \
  ! grep -F "A.ast" "$work/ocaml.err" >/dev/null; then
  printf 'build-ast-disappears-before-dependency-read: expected Rust=panic and OCaml=path-bearing rejection, got Rust=%s/OCaml=%s\n' \
    "$rust_status" "$ocaml_status" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.out" "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.out" "$work/ocaml.err" >&2
  exit 1
fi
checked=$((checked + 1))

"$rust" watch "$work/watch-config-rust" \
  >"$work/watch-rust.out" 2>"$work/watch-rust.err" &
rust_watch_pid=$!
background_pids="$background_pids $rust_watch_pid"
wait_for_file "$work/watch-config-rust/src/A.js"
wait_for_text "$work/watch-config-rust/lib/ocaml/.compiler.log" "#Done("
printf '{ invalid json\n' >"$work/watch-config-rust/rescript.json"
wait_for_text "$work/watch-rust.err" "Could not initialize build"
wait_for_exit "$rust_watch_pid"
set +e
wait "$rust_watch_pid"
rust_watch_status=$?
set -e
if [ "$rust_watch_status" -ne 101 ]; then
  printf 'watch-invalid-config-rebuild: expected Rust panic exit 101, got %s\n' \
    "$rust_watch_status" >&2
  cat "$work/watch-rust.out" "$work/watch-rust.err" >&2
  exit 1
fi

"$ocaml" watch "$work/watch-config-ocaml" \
  >"$work/watch-ocaml.out" 2>"$work/watch-ocaml.err" &
ocaml_watch_pid=$!
background_pids="$background_pids $ocaml_watch_pid"
wait_for_file "$work/watch-config-ocaml/src/A.js"
wait_for_text "$work/watch-config-ocaml/lib/ocaml/.compiler.log" "#Done("
printf '{ invalid json\n' >"$work/watch-config-ocaml/rescript.json"
wait_for_text "$work/watch-ocaml.err" "invalid JSON"
if ! kill -0 "$ocaml_watch_pid" 2>/dev/null; then
  echo "OCaml watcher exited after a recoverable config error" >&2
  cat "$work/watch-ocaml.out" "$work/watch-ocaml.err" >&2
  exit 1
fi
printf '{"name":"watch-config","sources":["src"],"package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}\n' \
  >"$work/watch-config-ocaml/rescript.json"
wait_for_file "$work/watch-config-ocaml/src/A.mjs"
wait_for_file "$work/watch-config-ocaml/lib/bs/build.ninja"
kill -TERM "$ocaml_watch_pid"
wait "$ocaml_watch_pid"
checked=$((checked + 1))

rust_filter_marker="$work/watch-filter-rust/after-build.log"
REWATCH_WATCH_FILTER_MARKER="$rust_filter_marker" \
  "$rust" watch --filter 'Include\.res$' \
    --after-build "node $work/watch-filter-marker.js" \
    "$work/watch-filter-rust" \
    >"$work/watch-filter-rust.out" 2>"$work/watch-filter-rust.err" &
rust_filter_pid=$!
background_pids="$background_pids $rust_filter_pid"
wait_for_file "$work/watch-filter-rust/src/Include.js"
wait_for_line_count "$rust_filter_marker" 1
if [ -e "$work/watch-filter-rust/src/Exclude.js" ]; then
  echo "Rust filter unexpectedly compiled the excluded source initially" >&2
  exit 1
fi
cp "$work/watch-filter-rust/src/Include.js" "$work/watch-filter-rust-initial.js"
printf 'let value = 2\n' >"$work/watch-filter-rust/src/Include.res"
printf 'let value = 11\n' >"$work/watch-filter-rust/src/Exclude.res"
wait_for_line_count "$rust_filter_marker" 2
if ! cmp -s "$work/watch-filter-rust-initial.js" \
    "$work/watch-filter-rust/src/Include.js"; then
  echo "Rust no longer reproduces the inverted watch-filter event behavior" >&2
  cat "$work/watch-filter-rust.out" "$work/watch-filter-rust.err" >&2
  exit 1
fi
kill -TERM "$rust_filter_pid"
wait "$rust_filter_pid"

ocaml_filter_marker="$work/watch-filter-ocaml/after-build.log"
REWATCH_WATCH_FILTER_MARKER="$ocaml_filter_marker" \
  "$ocaml" watch --filter 'Include\.res$' \
    --after-build "node $work/watch-filter-marker.js" \
    "$work/watch-filter-ocaml" \
    >"$work/watch-filter-ocaml.out" 2>"$work/watch-filter-ocaml.err" &
ocaml_filter_pid=$!
background_pids="$background_pids $ocaml_filter_pid"
wait_for_file "$work/watch-filter-ocaml/src/Include.js"
wait_for_line_count "$ocaml_filter_marker" 1
if [ -e "$work/watch-filter-ocaml/src/Exclude.js" ]; then
  echo "OCaml filter unexpectedly compiled the excluded source initially" >&2
  exit 1
fi
cp "$work/watch-filter-ocaml/src/Include.js" "$work/watch-filter-ocaml-initial.js"
printf 'let value = 2\n' >"$work/watch-filter-ocaml/src/Include.res"
wait_for_line_count "$ocaml_filter_marker" 2
if cmp -s "$work/watch-filter-ocaml-initial.js" \
    "$work/watch-filter-ocaml/src/Include.js" || \
  ! grep -F '2' "$work/watch-filter-ocaml/src/Include.js" >/dev/null; then
  echo "OCaml watch filter did not rebuild its included source" >&2
  cat "$work/watch-filter-ocaml.out" "$work/watch-filter-ocaml.err" >&2
  exit 1
fi
kill -TERM "$ocaml_filter_pid"
wait "$ocaml_filter_pid"
checked=$((checked + 1))

run_case clean-missing-dependency exit2 exit2 clean "$work/missing-dependency"
run_case clean-configless-dependency exit2 exit2 clean "$work/configless-dependency"
run_case clean-malformed-dependency exit2 exit2 clean "$work/malformed-dependency"
run_case watch-missing-dependency exit2 exit2 watch "$work/missing-dependency"
run_cwd_case format-missing-dependency exit2 exit2 \
  "$work/missing-dependency" format
run_cwd_case format-configless-dependency exit2 exit2 \
  "$work/configless-dependency" format
run_cwd_case format-malformed-dependency exit2 exit2 \
  "$work/malformed-dependency" format
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
