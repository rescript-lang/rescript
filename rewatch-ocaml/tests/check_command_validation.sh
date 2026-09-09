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
mkdir -p "$work/external-dev-source/src" \
  "$work/external-dev-source/node_modules/dep/src" \
  "$work/external-dev-source/node_modules/dep/test"
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
printf '{"name":"command-validation","sources":["src"]}\n' \
  >"$project/rescript.json"
printf 'let value = 1\n' >"$project/src/A.res"
printf 'not a ReScript source\n' >"$project/src/A.txt"
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
run_case build-excludes-external-dev-source accept accept build \
  "$work/external-dev-source"
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
