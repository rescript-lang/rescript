#!/bin/bash
set -eu

root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
rust=${1:-$root/rewatch/target/debug/rescript}
ocaml=${2:-$root/_build/default/rewatch-ocaml/rescript_ocaml.exe}
rust=$(realpath "$rust")
ocaml=$(realpath "$ocaml")
bsc_test_proxy="$root/_build/default/tests/rewatch_ounit_tests/rewatch_bsc_test_proxy.exe"
work=$(mktemp -d "${TMPDIR:-/tmp}/rewatch-command-validation-XXXXXX")
# Compiler diagnostics contain canonical paths. Resolve platform aliases such
# as macOS's /var -> /private/var before deriving paths used for comparison.
work=$(realpath "$work")
native_work=$work
native_short_work=$work
windows_posix_shell=false
case $(uname -s) in
  MINGW*|MSYS*)
    windows_posix_shell=true
    native_work=$(cd "$work" && pwd -W)
    native_user=$(cygpath -w "$USERPROFILE" | tr '\\' '/')
    native_short_user=$(cygpath -w -s "$USERPROFILE" | tr '\\' '/')
    native_short_work=${native_work/"$native_user"/"$native_short_user"}
    bsc_test_proxy=$(cygpath -aw \
      "$root/_build/default/tests/rewatch_ounit_tests/rewatch_bsc_test_proxy.exe")
    ;;
esac
delete_source_bsc=$bsc_test_proxy
delete_parse_sources_bsc=$bsc_test_proxy
delete_ast_bsc=$bsc_test_proxy
lock_owner_pid() {
  local shell_pid=$1
  if $windows_posix_shell; then
    # MSYS assigns a synthetic PID to native Windows children. Lock files are
    # consumed by native executables and therefore must contain the WINPID.
    ps -p "$shell_pid" -l | awk 'NR == 2 { print $4 }'
  else
    printf '%s\n' "$shell_pid"
  fi
}
directory_link() {
  local target=$1
  local link=$2
  if $windows_posix_shell; then
    LINK_PATH=$(cygpath -aw "$link") TARGET_PATH=$(cygpath -aw "$target") \
      powershell.exe -NoProfile -NonInteractive -Command \
        '$ErrorActionPreference = "Stop"; $null = New-Item -ItemType Junction -Path $env:LINK_PATH -Target $env:TARGET_PATH'
  else
    ln -s "$target" "$link"
  fi
}
file_link_if_supported() {
  local target=$1
  local link=$2
  if $windows_posix_shell; then
    LINK_PATH=$(cygpath -aw "$link") TARGET_PATH=$(cygpath -aw "$target") \
      powershell.exe -NoProfile -NonInteractive -Command \
        '$ErrorActionPreference = "Stop"; $null = New-Item -ItemType SymbolicLink -Path $env:LINK_PATH -Target $env:TARGET_PATH' \
        >/dev/null 2>&1
  else
    ln -s "$target" "$link"
  fi
}
command_work=$native_work
command_path() {
  case $1 in
    "$work"*) printf '%s\n' "$command_work${1#"$work"}" ;;
    *) printf '%s\n' "$1" ;;
  esac
}
terminate_and_wait() {
  local pid=$1
  local label=$2
  kill -TERM "$pid"
  set +e
  wait "$pid"
  local status=$?
  set -e
  if [ "$status" -ne 0 ] && \
    { ! $windows_posix_shell || [ "$status" -ne 143 ]; }; then
    printf '%s exited with status %s after shutdown\n' "$label" "$status" >&2
    return 1
  fi
}
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
mkdir -p "$work/watch-lock-order/src" "$work/watch-lock-order/lib"
mkdir -p "$work/signal-lock/src" "$work/signal-lock/lib"
mkdir -p "$work/signal-lock-owner/src"
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
mkdir -p "$work/active-permission/node_modules/a" \
  "$work/active-permission/node_modules/b"
mkdir -p "$work/source-path-file"
mkdir -p "$work/missing-runtime-package"
mkdir -p "$work/clean-missing-bsc/src" "$work/clean-missing-bsc/lib/bs"
mkdir -p "$work/clean-missing-runtime/src" \
  "$work/clean-missing-runtime/lib/bs"
for implementation in rust ocaml; do
  mkdir -p "$work/clean-duplicate-$implementation/src/one" \
    "$work/clean-duplicate-$implementation/src/two" \
    "$work/clean-duplicate-$implementation/lib/bs"
done
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
mkdir -p "$work/parse-source-race-rust/src" \
  "$work/parse-source-race-ocaml/src"
mkdir -p "$work/watch-config-rust/src" "$work/watch-config-ocaml/src"
mkdir -p "$work/watch-retained-graph/src"
mkdir -p "$work/watch-dependency-recovery/src" \
  "$work/watch-dependency-recovery/node_modules" \
  "$work/watch-dependency-recovery/packages/dep/src"
mkdir -p "$work/watch-dependency-install/src" \
  "$work/watch-dependency-install/node_modules"
mkdir -p "$work/watch-dependency-fallback/src" \
  "$work/watch-dependency-fallback/node_modules/dep" \
  "$work/watch-dependency-fallback/packages/dep/src" \
  "$work/node_modules"
mkdir -p "$work/watch-symlink-target/src" "$work/watch-symlink-external/sub"
mkdir -p "$work/watch-feature-scope/src" "$work/watch-feature-scope/inactive"
mkdir -p "$work/watch-filter-rust/src" "$work/watch-filter-rust/inactive" \
  "$work/watch-filter-ocaml/src" "$work/watch-filter-ocaml/inactive"
mkdir -p "$work/quiet-watch-rust/src" "$work/quiet-watch-ocaml/src"
printf '{"name":"command-validation","sources":["src"]}\n' \
  >"$project/rescript.json"
printf 'let value = 1\n' >"$project/src/A.res"
printf 'not a ReScript source\n' >"$project/src/A.txt"
printf '{"name":"signal-lock","sources":["src"]}\n' \
  >"$work/signal-lock/rescript.json"
printf 'let value = 1\n' >"$work/signal-lock/src/A.res"
printf '{"name":"signal-lock-owner","sources":["src"]}\n' \
  >"$work/signal-lock-owner/rescript.json"
printf 'let value = 1\n' >"$work/signal-lock-owner/src/A.res"
printf '{"name":"watch-lock-order","sources":["src"]}\n' \
  >"$work/watch-lock-order/rescript.json"
printf 'let value = 1\n' >"$work/watch-lock-order/src/A.res"
mkdir -p "$work/redirected-parse-fixture/src"
mkdir -p "$work/multiple-parse-errors/src"
mkdir -p "$work/redirected-config-diagnostics/src"
printf '{"name":"parse-output","sources":["src"]}\n' \
  >"$work/redirected-parse-fixture/rescript.json"
printf 'let value =\n' >"$work/redirected-parse-fixture/src/A.res"
printf '{"name":"multiple-parse-errors","sources":["src"]}\n' \
  >"$work/multiple-parse-errors/rescript.json"
printf 'let value =\n' >"$work/multiple-parse-errors/src/A.res"
printf 'let other =\n' >"$work/multiple-parse-errors/src/B.res"
printf '%s\n' \
  '{"name":"config-diagnostics","sources":["src"],"bsc-flags":[],"ignored-dirs":[],"future-field":true}' \
  >"$work/redirected-config-diagnostics/rescript.json"
printf 'let value = 1\n' >"$work/redirected-config-diagnostics/src/A.res"
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
  printf 'let value = 2\n' \
    >"$work/filter-basename-$implementation/src/nested/B2.res"
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
printf '{"name":"root","sources":[],"dependencies":["a","b"]}\n' \
  >"$work/active-permission/rescript.json"
printf '{"name":"a","sources":[],"allowed-dependents":["someone-else"]}\n' \
  >"$work/active-permission/node_modules/a/rescript.json"
printf '{"name":"b","sources":[],"allowed-dependents":["someone-else"]}\n' \
  >"$work/active-permission/node_modules/b/rescript.json"
printf '{"name":"source-path-file","sources":["src"]}\n' \
  >"$work/source-path-file/rescript.json"
printf 'not a directory\n' >"$work/source-path-file/src"
printf '{"name":"missing-runtime-package","sources":[]}\n' \
  >"$work/missing-runtime-package/rescript.json"
for clean_project in clean-missing-bsc clean-missing-runtime; do
  printf '{"name":"%s","sources":["src"]}\n' "$clean_project" \
    >"$work/$clean_project/rescript.json"
  printf 'let value = 1\n' >"$work/$clean_project/src/A.res"
  printf 'owned compiler artifact\n' >"$work/$clean_project/lib/bs/marker"
done
for implementation in rust ocaml; do
  duplicate_clean="$work/clean-duplicate-$implementation"
  printf '%s\n' \
    '{"name":"duplicate-clean","sources":{"dir":"src","subdirs":true},"package-specs":{"module":"esmodule","in-source":true}}' \
    >"$duplicate_clean/rescript.json"
  printf 'let value = 1\n' >"$duplicate_clean/src/one/A.res"
  printf 'let value = 2\n' >"$duplicate_clean/src/two/A.res"
  printf 'generated\n' >"$duplicate_clean/src/one/A.js"
  printf 'generated\n' >"$duplicate_clean/src/two/A.js"
  printf 'owned compiler artifact\n' >"$duplicate_clean/lib/bs/marker"
done
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
directory_link "$work/format-source-selection/packages/local" \
  "$work/format-source-selection/node_modules/local"
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
printf '{"name":"parse-source-race","sources":["src"]}\n' \
  >"$work/parse-source-race-rust/rescript.json"
cp "$work/parse-source-race-rust/rescript.json" \
  "$work/parse-source-race-ocaml/rescript.json"
for implementation in rust ocaml; do
  printf 'let value = 1\n' \
    >"$work/parse-source-race-$implementation/src/A.res"
  printf 'let value = 2\n' \
    >"$work/parse-source-race-$implementation/src/B.res"
done
printf '{"name":"watch-config","sources":["src"]}\n' \
  >"$work/watch-config-rust/rescript.json"
cp "$work/watch-config-rust/rescript.json" \
  "$work/watch-config-ocaml/rescript.json"
printf 'let value = 1\n' >"$work/watch-config-rust/src/A.res"
cp "$work/watch-config-rust/src/A.res" "$work/watch-config-ocaml/src/A.res"
printf '{"name":"watch-retained-graph","sources":["src"]}\n' \
  >"$work/watch-retained-graph/rescript.json"
printf 'let value = 1\n' >"$work/watch-retained-graph/src/A.res"
printf 'let oldValue = 1\n' >"$work/watch-retained-graph/src/C.res"
printf 'let value = A.value\n' >"$work/watch-retained-graph/src/B.res"
printf '{"name":"watch-dependency-recovery","sources":["src"],"dependencies":["dep"]}\n' \
  >"$work/watch-dependency-recovery/rescript.json"
printf 'let value = 1\n' >"$work/watch-dependency-recovery/src/A.res"
printf '{ invalid json\n' \
  >"$work/watch-dependency-recovery/packages/dep/rescript.json"
printf 'let dependency = 1\n' \
  >"$work/watch-dependency-recovery/packages/dep/src/Dep.res"
directory_link "$work/watch-dependency-recovery/packages/dep" \
  "$work/watch-dependency-recovery/node_modules/dep"
printf '{"name":"watch-dependency-install","sources":["src"],"dependencies":["@scope/dep"]}\n' \
  >"$work/watch-dependency-install/rescript.json"
printf 'let value = 1\n' >"$work/watch-dependency-install/src/A.res"
printf '{"name":"watch-feature-scope","sources":["src",{"dir":"inactive","feature":"inactive"}]}\n' \
  >"$work/watch-feature-scope/rescript.json"
printf 'let value = 1\n' >"$work/watch-feature-scope/src/A.res"
printf 'let inactive = 1\n' >"$work/watch-feature-scope/inactive/Inactive.res"
printf '{"name":"watch-dependency-fallback","sources":["src"],"dependencies":["dep"]}\n' \
  >"$work/watch-dependency-fallback/rescript.json"
printf 'let value = 1\n' >"$work/watch-dependency-fallback/src/A.res"
printf '{"name":"dep","sources":["src"]}\n' \
  >"$work/watch-dependency-fallback/packages/dep/rescript.json"
printf 'let dependency = 1\n' \
  >"$work/watch-dependency-fallback/packages/dep/src/Dep.res"
printf '{"name":"watch-symlink-target","sources":["src"]}\n' \
  >"$work/watch-symlink-target/rescript.json"
printf 'let linked = 1\n' >"$work/watch-symlink-external/sub/Linked.res"
file_symlinks_supported=true
if ! file_link_if_supported "$work/watch-symlink-external/sub/Linked.res" \
    "$work/watch-symlink-target/src/Linked.res"; then
  file_symlinks_supported=false
fi
for implementation in rust ocaml; do
  printf '{"name":"watch-filter","sources":["src",{"dir":"inactive","feature":"inactive"}]}\n' \
    >"$work/watch-filter-$implementation/rescript.json"
  printf 'let value = 1\n' \
    >"$work/watch-filter-$implementation/src/Include.res"
  printf 'let value = 10\n' \
    >"$work/watch-filter-$implementation/src/Exclude.res"
  printf 'let value = 20\n' \
    >"$work/watch-filter-$implementation/inactive/Inactive.res"
  printf '{"name":"quiet-watch","sources":["src"]}\n' \
    >"$work/quiet-watch-$implementation/rescript.json"
  printf 'let value = 1\n' \
    >"$work/quiet-watch-$implementation/src/A.res"
done
printf 'require("fs").appendFileSync(process.env.REWATCH_WATCH_FILTER_MARKER, "done\\n")\n' \
  >"$work/watch-filter-marker.js"
printf '%s\n' \
  '#!/bin/sh' \
  'if [ -f "$REWATCH_SCOPE_BLOCK_REQUEST" ] && [ ! -f "$REWATCH_SCOPE_BLOCK_STARTED" ]; then' \
  '  : >"$REWATCH_SCOPE_BLOCK_STARTED"' \
  '  attempts=0' \
  '  while [ ! -f "$REWATCH_SCOPE_BLOCK_RELEASE" ] && [ "$attempts" -lt 200 ]; do' \
  '    attempts=$((attempts + 1))' \
  '    sleep 0.05' \
  '  done' \
  'fi' \
  'exec "$REWATCH_SCOPE_REAL_BSC" "$@"' \
  >"$work/watch-scope-bsc.sh"
chmod +x "$work/watch-scope-bsc.sh"
watch_scope_bsc="$work/watch-scope-bsc.sh"
if $windows_posix_shell; then
  watch_scope_bsc=$bsc_test_proxy
fi

default_bsc=$root/_build/default/compiler/bsc/rescript_compiler_main.exe
default_runtime=$root/packages/@rescript/runtime
case $(uname -s) in
  MINGW*|MSYS*)
    default_bsc=$(cygpath -w "$default_bsc")
    default_runtime=$(cygpath -w "$default_runtime")
    ;;
esac
export RESCRIPT_BSC_EXE=${RESCRIPT_BSC_EXE:-$default_bsc}
export RESCRIPT_RUNTIME=${RESCRIPT_RUNTIME:-$default_runtime}

classify() {
  case "$1" in
    0) printf accept ;;
    101) printf panic ;;
    2) printf exit2 ;;
    *) printf reject ;;
  esac
}

strip_ansi() {
  LC_ALL=C sed $'s/\033\\[[0-9;]*m//g' "$1"
}

normalize_project_path() {
  local input=$1
  local output=$2
  local project=$3
  local native_project=$project
  local native_short_project=$project
  case $(uname -s) in
    MINGW*|MSYS*)
      native_project=$(cd "$project" && pwd -W)
      native_short_project=${native_project/"$native_user"/"$native_short_user"}
      ;;
  esac
  PROJECT_PATH=$project PROJECT_NATIVE_PATH=$native_project \
    PROJECT_SHORT_PATH=$native_short_project node -e '
    const fs = require("fs");
    let text = fs.readFileSync(process.argv[1], "utf8");
    for (const project of [process.env.PROJECT_PATH, process.env.PROJECT_NATIVE_PATH, process.env.PROJECT_SHORT_PATH]) {
      for (const spelling of [project, project.replaceAll("/", "\\")]) {
        text = text.split(spelling).join("<ROOT>");
      }
    }
    text = text.replaceAll("\\", "/");
    text = text.replaceAll("./<ROOT>/", "./");
    fs.writeFileSync(process.argv[2], text);
  ' "$input" "$output"
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
  local name=$1
  local fragment=$2
  local native_fragment=${fragment/"$work"/"$native_work"}
  local native_short_fragment=${fragment/"$work"/"$native_short_work"}
  if ! { grep -F "$fragment" "$work/rust.err" >/dev/null || \
      tr '\\' '/' <"$work/rust.err" | grep -F "$native_fragment" >/dev/null || \
      tr '\\' '/' <"$work/rust.err" | grep -F "$native_short_fragment" >/dev/null; } || \
    ! { grep -F "$fragment" "$work/ocaml.err" >/dev/null || \
      tr '\\' '/' <"$work/ocaml.err" | grep -F "$native_fragment" >/dev/null || \
      tr '\\' '/' <"$work/ocaml.err" | grep -F "$native_short_fragment" >/dev/null; }; then
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

run_build_output_case() {
  name=$1
  expected_status=$2
  fixture=$3
  mode=$4
  rust_project="$work/$name-rust"
  ocaml_project="$work/$name-ocaml"
  cp -R "$fixture" "$rust_project"
  cp -R "$fixture" "$ocaml_project"
  set +e
  if [ "$mode" = quiet ]; then
    "$rust" -q build "$rust_project" >"$work/rust.out" 2>"$work/rust.err"
    rust_status=$?
    "$ocaml" -q build "$ocaml_project" >"$work/ocaml.out" 2>"$work/ocaml.err"
    ocaml_status=$?
  elif [ "$mode" = forced-color ]; then
    CLICOLOR_FORCE=1 "$rust" build "$rust_project" \
      >"$work/rust.out" 2>"$work/rust.err"
    rust_status=$?
    CLICOLOR_FORCE=1 "$ocaml" build "$ocaml_project" \
      >"$work/ocaml.out" 2>"$work/ocaml.err"
    ocaml_status=$?
  else
    "$rust" build "$rust_project" >"$work/rust.out" 2>"$work/rust.err"
    rust_status=$?
    "$ocaml" build "$ocaml_project" >"$work/ocaml.out" 2>"$work/ocaml.err"
    ocaml_status=$?
  fi
  set -e
  normalize_project_path "$work/rust.out" "$work/rust.out.norm" "$rust_project"
  normalize_project_path "$work/rust.err" "$work/rust.err.norm" "$rust_project"
  normalize_project_path "$work/ocaml.out" "$work/ocaml.out.norm" "$ocaml_project"
  normalize_project_path "$work/ocaml.err" "$work/ocaml.err.norm" "$ocaml_project"
  if [ "$rust_status" -ne "$expected_status" ] || \
    [ "$ocaml_status" -ne "$expected_status" ] || \
    ! cmp -s "$work/rust.out.norm" "$work/ocaml.out.norm" || \
    ! cmp -s "$work/rust.err.norm" "$work/ocaml.err.norm"; then
    echo "$name: $mode build output differs" >&2
    printf '%s\n' \
      "statuses: expected=$expected_status Rust=$rust_status OCaml=$ocaml_status" \
      >&2
    printf '%s\n' '--- Rust stdout ---' >&2
    cat "$work/rust.out.norm" >&2
    printf '%s\n' '--- OCaml stdout ---' >&2
    cat "$work/ocaml.out.norm" >&2
    printf '%s\n' '--- Rust stderr ---' >&2
    cat "$work/rust.err.norm" >&2
    printf '%s\n' '--- OCaml stderr ---' >&2
    cat "$work/ocaml.err.norm" >&2
    for stream in out err; do
      if ! cmp -s "$work/rust.$stream.norm" "$work/ocaml.$stream.norm"; then
        printf '%s\n' "--- $stream byte counts ---" >&2
        wc -c "$work/rust.$stream.norm" "$work/ocaml.$stream.norm" >&2
        printf '%s\n' "--- first differing $stream bytes ---" >&2
        cmp -l "$work/rust.$stream.norm" "$work/ocaml.$stream.norm" \
          | head -n 20 >&2 || true
        printf '%s\n' "--- Rust $stream tail bytes ---" >&2
        tail -c 64 "$work/rust.$stream.norm" | od -An -tx1c >&2
        printf '%s\n' "--- OCaml $stream tail bytes ---" >&2
        tail -c 64 "$work/ocaml.$stream.norm" | od -An -tx1c >&2
      fi
    done
    exit 1
  fi
  checked=$((checked + 1))
}

run_multiple_parse_errors_case() {
  rust_project="$work/multiple-parse-errors-rust"
  ocaml_project="$work/multiple-parse-errors-ocaml"
  cp -R "$work/multiple-parse-errors" "$rust_project"
  cp -R "$work/multiple-parse-errors" "$ocaml_project"
  set +e
  "$rust" build "$rust_project" >"$work/rust.out" 2>"$work/rust.err"
  rust_status=$?
  "$ocaml" build "$ocaml_project" >"$work/ocaml.out" 2>"$work/ocaml.err"
  ocaml_status=$?
  set -e
  if [ "$rust_status" -ne 1 ] || [ "$ocaml_status" -ne 1 ]; then
    printf 'multiple-parse-errors: expected status 1, got Rust=%s OCaml=%s\n' \
      "$rust_status" "$ocaml_status" >&2
    exit 1
  fi
  for implementation in rust ocaml; do
    error="$work/$implementation.err"
    if [ "$(grep -cF 'Error in multiple-parse-errors:' "$error")" -ne 2 ] || \
      ! grep -E '[/\\]src[/\\]A\.res' "$error" >/dev/null || \
      ! grep -E '[/\\]src[/\\]B\.res' "$error" >/dev/null; then
      echo "$implementation did not retain every independent parse error" >&2
      cat "$error" >&2
      exit 1
    fi
  done
  checked=$((checked + 1))
}

wait_for_file() {
  path=$1
  attempts=0
  while [ "$attempts" -lt 150 ] && [ ! -f "$path" ]; do
    attempts=$((attempts + 1))
    sleep 0.1
  done
  if [ -f "$path" ]; then
    return 0
  fi
  printf 'Timed out waiting for file %s (requested at line %s)\n' \
    "$path" "${BASH_LINENO[0]:-unknown}" >&2
  return 1
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
  if grep -F "$pattern" "$path" >/dev/null 2>&1; then
    return 0
  fi
  printf 'Timed out waiting for %s in %s\n' "$pattern" "$path" >&2
  if [ -f "$path" ]; then
    printf '%s\n' '--- observed contents ---' >&2
    cat "$path" >&2
  else
    printf '%s\n' '--- file does not exist ---' >&2
  fi
  return 1
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

line_count_stays() {
  path=$1
  expected=$2
  attempts=0
  while [ "$attempts" -lt 20 ]; do
    actual=0
    if [ -f "$path" ]; then
      actual=$(wc -l <"$path" | tr -d ' ')
    fi
    if [ "$actual" -ne "$expected" ]; then
      return 1
    fi
    attempts=$((attempts + 1))
    sleep 0.1
  done
}

run_build_output_case redirected-success 0 \
  "$root/rewatch-ocaml/tests/basic" redirected
run_build_output_case redirected-compile-error 1 \
  "$root/rewatch-ocaml/tests/failure" redirected
run_build_output_case redirected-parse-error 1 \
  "$work/redirected-parse-fixture" redirected
run_multiple_parse_errors_case
run_build_output_case redirected-warning 0 \
  "$root/rewatch-ocaml/tests/warning-replay" redirected
run_build_output_case redirected-config-diagnostics 0 \
  "$work/redirected-config-diagnostics" redirected
run_build_output_case forced-color-config-diagnostics 0 \
  "$work/redirected-config-diagnostics" forced-color

run_build_output_case quiet-success 0 "$project" quiet
if [ -s "$work/rust.out" ] || [ -s "$work/rust.err" ] || \
  [ -s "$work/ocaml.out" ] || [ -s "$work/ocaml.err" ]; then
  echo "quiet-success: a clean build emitted output" >&2
  for implementation in rust ocaml; do
    printf '%s\n' "--- $implementation stdout ---" >&2
    cat "$work/$implementation.out" >&2
    printf '%s\n' "--- $implementation stderr ---" >&2
    cat "$work/$implementation.err" >&2
  done
  exit 1
fi
run_build_output_case quiet-compile-error 1 \
  "$root/rewatch-ocaml/tests/failure" quiet
run_build_output_case quiet-parse-error 1 "$work/redirected-parse-fixture" quiet
run_build_output_case quiet-warning 0 \
  "$root/rewatch-ocaml/tests/warning-replay" quiet

run_case build-subcommand-version exit2 exit2 build --version
run_case clustered-global-version accept accept -vV build
run_case clustered-global-verbosity accept accept -vvvvv build "$project"
run_case clustered-global-version-before-help accept accept -Vh build
run_case clustered-global-help-before-version accept accept -hV build
run_case clustered-subcommand-help-before-version accept accept build -hV
run_case clustered-subcommand-version-before-help exit2 exit2 build -Vh
run_case implicit-conflicting-verbosity exit2 exit2 -v -q
run_case build-conflicting-verbosity exit2 exit2 build --verbose --quiet
run_case filter-perl-quoting exit2 exit2 build --filter '\QFoo.res\E' "$project"
run_case filter-comment-group exit2 exit2 build --filter '(?#note)Foo' "$project"
run_case filter-octal-escape exit2 exit2 build --filter '\123' "$project"
run_case filter-braced-octal-escape exit2 exit2 build --filter '\o{123}' "$project"
run_case filter-perl-control-escape-e exit2 exit2 build --filter '\e' "$project"
run_case filter-perl-anchor-g exit2 exit2 build --filter '\G' "$project"
run_case filter-perl-anchor-z exit2 exit2 build --filter '\Z' "$project"
run_case filter-descending-range exit2 exit2 build --filter '[z-a]' "$project"
run_case filter-escaped-range-start exit2 exit2 build --filter '[\d-z]' "$project"
run_case filter-in-class-nonword accept exit2 build --filter \
  '^[\W]+\.res$' "$project"
run_case filter-collating-element accept exit2 build --filter '[[.a.]]' "$project"
run_case filter-class-algebra accept exit2 build --filter \
  '[a-z&&[^aeiou]]' "$project"
run_case build-no-timing-consumes-folder exit2 exit2 build --no-timing "$project"
run_case compiler-args-source accept accept compiler-args "$project/src/A.res"
run_case compiler-args-extension accept reject compiler-args "$project/src/A.txt"
run_case compiler-args-missing panic reject compiler-args "$project/src/Missing.res"
run_case compiler-args-no-project panic reject compiler-args "$work/orphan/A.res"

run_missing_bsc_case build-missing-bsc build "$project"
run_missing_bsc_case format-missing-bsc format "$project/src/A.res"
set +e
RESCRIPT_BSC_EXE="$work/missing-bsc" \
  "$rust" clean "$work/clean-missing-bsc" \
  >"$work/rust.out" 2>"$work/rust.err"
rust_status=$?
RESCRIPT_BSC_EXE="$work/missing-bsc" \
  "$ocaml" clean "$work/clean-missing-bsc" \
  >"$work/ocaml.out" 2>"$work/ocaml.err"
ocaml_status=$?
set -e
if [ "$(classify "$rust_status")" != panic ] || [ "$ocaml_status" -ne 0 ] || \
  [ -e "$work/clean-missing-bsc/lib/bs/marker" ]; then
  echo "clean-missing-bsc: expected Rust panic and successful OCaml cleanup" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.out" "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.out" "$work/ocaml.err" >&2
  exit 1
fi
checked=$((checked + 1))
set +e
env -u RESCRIPT_RUNTIME "$rust" clean "$work/clean-missing-runtime" \
  >"$work/rust.out" 2>"$work/rust.err"
rust_status=$?
env -u RESCRIPT_RUNTIME "$ocaml" clean "$work/clean-missing-runtime" \
  >"$work/ocaml.out" 2>"$work/ocaml.err"
ocaml_status=$?
set -e
if [ "$(classify "$rust_status")" != reject ] || [ "$ocaml_status" -ne 0 ] || \
  [ -e "$work/clean-missing-runtime/lib/bs/marker" ]; then
  echo "clean-missing-runtime: expected Rust rejection and successful OCaml cleanup" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.out" "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.out" "$work/ocaml.err" >&2
  exit 1
fi
checked=$((checked + 1))
set +e
env -u RESCRIPT_RUNTIME "$rust" build "$work/missing-runtime-package" \
  >"$work/rust.out" 2>"$work/rust.err"
rust_status=$?
env -u RESCRIPT_RUNTIME "$ocaml" build "$work/missing-runtime-package" \
  >"$work/ocaml.out" 2>"$work/ocaml.err"
ocaml_status=$?
set -e
if [ "$rust_status" -eq 0 ] || [ "$ocaml_status" -eq 0 ]; then
  echo "build-missing-runtime-package: expected both builds to reject" >&2
  exit 1
fi
require_both_errors_contain build-missing-runtime-package \
  'The rescript runtime package could not be found.'
require_both_errors_contain build-missing-runtime-package \
  'Please set RESCRIPT_RUNTIME environment variable'
checked=$((checked + 1))
set +e
RESCRIPT_RUNTIME="$work/missing-runtime" "$rust" build "$project" \
  >"$work/rust.out" 2>"$work/rust.err"
rust_status=$?
RESCRIPT_RUNTIME="$work/missing-runtime" "$ocaml" build "$project" \
  >"$work/ocaml.out" 2>"$work/ocaml.err"
ocaml_status=$?
set -e
if [ "$rust_status" -eq 0 ] || [ "$ocaml_status" -eq 0 ] || \
  ! grep -F "RESCRIPT_RUNTIME points to missing path" \
    "$work/ocaml.err" >/dev/null || \
  ! grep -F "missing-runtime" "$work/ocaml.err" >/dev/null || \
  ! grep -F "The module or file Pervasives can't be found." \
    "$work/rust.err" >/dev/null; then
  echo "build-stale-runtime: expected contextual OCaml preflight rejection" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.out" "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.out" "$work/ocaml.err" >&2
  exit 1
fi
checked=$((checked + 1))
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
if ! grep -E 'Is a directory|Access is denied' "$work/rust.err" >/dev/null || \
  ! grep -F 'Is a directory' "$work/ocaml.err" >/dev/null; then
  echo "format-config-directory: file-kind diagnostic was lost" >&2
  exit 1
fi
set +e
printf 'let value =\n' | "$rust" format --stdin .res \
  >"$work/rust.out" 2>"$work/rust.err"
rust_status=$?
printf 'let value =\n' | "$ocaml" format --stdin .res \
  >"$work/ocaml.out" 2>"$work/ocaml.err"
ocaml_status=$?
set -e
if [ "$rust_status" -eq 0 ] || [ "$ocaml_status" -eq 0 ] || \
  [ -s "$work/rust.out" ] || [ -s "$work/ocaml.out" ]; then
  echo "format-invalid-stdin: expected both formatters to reject" >&2
  exit 1
fi
require_both_errors_contain format-invalid-stdin 'Error formatting stdin:'
require_both_errors_contain format-invalid-stdin \
  'This let-binding misses an expression'
checked=$((checked + 1))
mkdir -p "$work/format-write-rust" "$work/format-write-ocaml"
printf 'let value=1\n' >"$work/format-write-rust/A.res"
printf 'let value=1\n' >"$work/format-write-ocaml/A.res"
chmod 0444 "$work/format-write-rust/A.res" "$work/format-write-ocaml/A.res"
if [ ! -w "$work/format-write-rust/A.res" ] && \
  [ ! -w "$work/format-write-ocaml/A.res" ]; then
  set +e
  "$rust" format "$work/format-write-rust/A.res" \
    >"$work/rust.out" 2>"$work/rust.err"
  rust_status=$?
  "$ocaml" format "$work/format-write-ocaml/A.res" \
    >"$work/ocaml.out" 2>"$work/ocaml.err"
  ocaml_status=$?
  set -e
  if [ "$rust_status" -eq 0 ] || [ "$ocaml_status" -eq 0 ] || \
    ! grep -E 'Permission denied|Access is denied' "$work/rust.err" >/dev/null || \
    ! grep -F "Could not write formatted file" "$work/ocaml.err" >/dev/null || \
    ! grep -F "format-write-ocaml/A.res" \
      < <(tr '\\' '/' <"$work/ocaml.err") >/dev/null; then
    echo "format-write-failure: formatter write failures lost context" >&2
    printf '%s\n' '--- Rust output ---' >&2
    cat "$work/rust.out" "$work/rust.err" >&2
    printf '%s\n' '--- OCaml output ---' >&2
    cat "$work/ocaml.out" "$work/ocaml.err" >&2
    exit 1
  fi
  checked=$((checked + 1))
fi
chmod 0644 "$work/format-write-rust/A.res" "$work/format-write-ocaml/A.res"

run_case build-missing-folder reject reject build "$work/missing"
run_case build-existing-folder-without-config reject reject build "$work/empty"
require_both_errors_contain build-existing-folder-without-config "$work/empty"
run_case build-malformed-config reject reject build "$work/malformed"
require_both_errors_contain build-malformed-config \
  "$work/malformed"
run_case build-malformed-parent reject reject build "$work/malformed-parent/child"
require_both_errors_contain build-malformed-parent \
  "$work/malformed-parent"
run_case build-config-path-is-directory reject reject build "$work/config-directory"
require_both_errors_contain build-config-path-is-directory \
  "$work/config-directory"
run_case after-build-nonzero-is-not-ignored accept reject build --after-build \
  "node $command_work/failing-after-build.js" "$project"
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
require_both_errors_contain js-post-build-nonzero "$js_post_build_error"
printf 'not-a-pid' >"$work/malformed-lock/lib/build.lock"
run_case build-malformed-lock reject reject build "$work/malformed-lock"
if [ "$(cat "$work/malformed-lock/lib/build.lock")" != not-a-pid ]; then
  echo "OCaml replaced a malformed build lock with unknown ownership" >&2
  exit 1
fi
if [ -n "$(find "$work/malformed-lock/lib" -maxdepth 1 \
  -name '.build-lock-*.tmp' -print -quit)" ]; then
  echo "OCaml left a build-lock candidate after acquisition failed" >&2
  exit 1
fi
# Use the real executable as the lock owner. Copying an MSYS utility to a
# rescript-prefixed name does not change the image name reported by tasklist,
# while both implementations deliberately reject locks owned by unrelated
# executables to protect against PID reuse.
"$ocaml" watch "$work/signal-lock-owner" \
  >"$work/signal-lock-owner.out" 2>"$work/signal-lock-owner.err" &
signal_lock_owner_pid=$!
background_pids="$background_pids $signal_lock_owner_pid"
wait_for_text "$work/signal-lock-owner.out" "Finished initial compilation"
printf '%s' "$(lock_owner_pid "$signal_lock_owner_pid")" \
  >"$work/signal-lock/lib/build.lock"
"$ocaml" watch "$work/signal-lock" \
  >"$work/signal-lock.out" 2>"$work/signal-lock.err" &
signal_lock_watch_pid=$!
background_pids="$background_pids $signal_lock_watch_pid"
wait_for_text "$work/signal-lock.out" "Waiting for other build to finish"
kill -TERM "$signal_lock_watch_pid"
set +e
wait "$signal_lock_watch_pid"
signal_lock_status=$?
set -e
if ! $windows_posix_shell && [ "$signal_lock_status" -ne 0 ]; then
  echo "Signal during build-lock wait exited with $signal_lock_status" >&2
  exit 1
fi
# MSYS implements kill for native processes by terminating them externally,
# so no Windows console control event reaches OCaml and the status is nonzero.
# The native Windows unit tests cover deferred signal dispatch; this integration
# case still proves that terminating a waiter cannot emit a spurious diagnostic.
if [ -s "$work/signal-lock.err" ]; then
  echo "Signal during build-lock wait emitted a diagnostic" >&2
  cat "$work/signal-lock.err" >&2
  exit 1
fi
kill -TERM "$signal_lock_owner_pid"
wait "$signal_lock_owner_pid" 2>/dev/null || true
checked=$((checked + 1))
printf 'not-a-pid' >"$work/malformed-lock/lib/watch.lock"
run_case watch-malformed-lock reject reject watch "$work/malformed-lock"
if [ "$(cat "$work/malformed-lock/lib/watch.lock")" != not-a-pid ]; then
  echo "OCaml replaced a malformed watch lock with unknown ownership" >&2
  exit 1
fi
if [ -n "$(find "$work/malformed-lock/lib" -maxdepth 1 \
  -name '.watch-lock-*.tmp' -print -quit)" ]; then
  echo "OCaml left a watch-lock candidate after acquisition failed" >&2
  exit 1
fi
printf '{ invalid json\n' >"$work/watch-lock-order/rescript.json"
# A copied MSYS utility keeps its original Windows image name and is therefore
# rejected as an unrelated lock owner. Run the implementation under test as a
# real watcher so both process identity and the native PID match production.
for implementation in rust ocaml; do
  case "$implementation" in
    rust) executable=$rust ;;
    ocaml) executable=$ocaml ;;
  esac
  owner_dir="$work/watch-lock-owner-$implementation"
  mkdir -p "$owner_dir/src"
  printf '{"name":"watch-lock-owner-%s","sources":["src"]}\n' \
    "$implementation" >"$owner_dir/rescript.json"
  printf 'let value = 1\n' >"$owner_dir/src/A.res"
  "$executable" watch "$owner_dir" \
    >"$owner_dir/watch.out" 2>"$owner_dir/watch.err" &
  watch_lock_owner_pid=$!
  background_pids="$background_pids $watch_lock_owner_pid"
  wait_for_text "$owner_dir/watch.out" "Finished initial compilation"
  native_owner_pid=$(lock_owner_pid "$watch_lock_owner_pid")
  if [ -z "$native_owner_pid" ]; then
    echo "watch-lock-before-config: could not resolve $implementation owner PID" >&2
    exit 1
  fi
  printf '%s' "$native_owner_pid" \
    >"$work/watch-lock-order/lib/watch.lock"
  set +e
  "$executable" watch "$work/watch-lock-order" \
    >"$work/$implementation.out" 2>"$work/$implementation.err"
  status=$?
  set -e
  if [ "$(classify "$status")" != reject ] || \
    ! grep -F 'A ReScript build is already running' \
      "$work/$implementation.err" >/dev/null || \
    grep -F 'invalid JSON' "$work/$implementation.err" >/dev/null; then
    echo "watch-lock-before-config: $implementation lock acquisition did not precede config parsing" >&2
    cat "$work/$implementation.out" "$work/$implementation.err" >&2
    exit 1
  fi
  kill -TERM "$watch_lock_owner_pid"
  wait "$watch_lock_owner_pid" 2>/dev/null || true
done
checked=$((checked + 1))
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
set +e
"$rust" build --filter '(?:A|B\d)\.res$' "$work/filter-basename-rust" \
  >"$work/rust.out" 2>"$work/rust.err"
rust_status=$?
"$ocaml" build --filter '(?:A|B\d)\.res$' "$work/filter-basename-ocaml" \
  >"$work/ocaml.out" 2>"$work/ocaml.err"
ocaml_status=$?
set -e
if [ "$(classify "$rust_status")" != accept ] || \
  [ "$(classify "$ocaml_status")" != accept ] || \
  [ ! -e "$work/filter-basename-rust/src/nested/B2.js" ] || \
  [ ! -e "$work/filter-basename-ocaml/src/nested/B2.js" ]; then
  echo "Source filters did not consistently support Rust-style regex syntax" >&2
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
strip_ansi "$work/rust.out" >"$work/rust.out.plain"
strip_ansi "$work/rust.err" >"$work/rust.err.plain"
if ! grep -F 'a has the following unallowed dependencies' \
    "$work/rust.err.plain" >/dev/null; then
  echo "Rust dormant external dev-dependency rejection was not reproduced" >&2
  cat "$work/rust.out" "$work/rust.err" >&2
  exit 1
fi
run_case build-reports-all-active-permission-failures reject reject build \
  "$work/active-permission"
strip_ansi "$work/rust.out" >"$work/rust.out.plain"
strip_ansi "$work/rust.err" >"$work/rust.err.plain"
strip_ansi "$work/ocaml.out" >"$work/ocaml.out.plain"
strip_ansi "$work/ocaml.err" >"$work/ocaml.err.plain"
rust_permission_details=$(grep -Ec '^dependencies dependencies: (a|b)$' \
  "$work/rust.out.plain" || true)
if [ "$rust_permission_details" -ne 1 ] || \
  ! grep -Fx 'root dependencies: a' "$work/ocaml.err.plain" >/dev/null || \
  ! grep -Fx 'root dependencies: b' "$work/ocaml.err.plain" >/dev/null || \
  ! grep -F 'unallowed_dependents' "$work/rust.err.plain" >/dev/null || \
  ! grep -F 'config.json' "$work/rust.err.plain" >/dev/null || \
  ! grep -F 'Update allowed-dependents in the dependency rescript.json files.' \
    "$work/ocaml.err.plain" >/dev/null || [ -s "$work/ocaml.out.plain" ]; then
  echo "Active dependency-permission diagnostics changed unexpectedly" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.out" "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.out" "$work/ocaml.err" >&2
  exit 1
fi
run_case build-source-path-is-file accept accept build "$work/source-path-file"
if ! cmp -s "$work/rust.err" "$work/ocaml.err" || \
  ! grep -F 'Could not read folder: "src"' "$work/ocaml.err" >/dev/null; then
  echo "Non-directory source diagnostics differ" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.out" "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.out" "$work/ocaml.err" >&2
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
run_cwd_case format-scans-graph-with-effective-features \
  reject reject \
  "$work/format-source-selection" format --check
normalize_project_path "$work/rust.err" "$work/rust.err.norm" \
  "$work/format-source-selection"
normalize_project_path "$work/ocaml.err" "$work/ocaml.err.norm" \
  "$work/format-source-selection"
missing_folder='Could not read folder: "missing". Specified in dependency: installed'
base_file='[format check] <ROOT>/packages/local/base/Base.res'
native_file='[format check] <ROOT>/packages/local/native/Native.res'
other_file='<ROOT>/packages/local/other/Other.res'
if ! grep -F "$missing_folder" "$work/rust.err.norm" >/dev/null || \
  ! grep -F "$missing_folder" "$work/ocaml.err.norm" >/dev/null || \
  ! grep -F "$base_file" "$work/rust.err.norm" >/dev/null || \
  ! grep -F "$base_file" "$work/ocaml.err.norm" >/dev/null || \
  ! grep -F "$native_file" "$work/rust.err.norm" >/dev/null || \
  ! grep -F "$native_file" "$work/ocaml.err.norm" >/dev/null || \
  grep -F "$other_file" "$work/rust.err.norm" >/dev/null || \
  grep -F "$other_file" "$work/ocaml.err.norm" >/dev/null || \
  ! grep -F 'The 2 files listed above need formatting' "$work/rust.err.norm" >/dev/null || \
  ! grep -F 'The 2 files listed above need formatting' "$work/ocaml.err.norm" >/dev/null; then
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
normalize_project_path "$work/rust.err" "$work/rust.err.norm" \
  "$work/package-name-mismatch"
normalize_project_path "$work/ocaml.err" "$work/ocaml.err.norm" \
  "$work/package-name-mismatch"
if ! cmp -s "$work/rust.err.norm" "$work/ocaml.err.norm"; then
  echo "Package-name mismatch diagnostics differ" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.err" >&2
  exit 1
fi
run_case build-malformed-package-json reject reject build \
  "$work/malformed-package-json"
require_both_errors_contain build-malformed-package-json \
  'Could not initialize build: Could not parse package.json:'
run_case build-mismatched-dependency-name panic exit2 build \
  "$work/mismatched-dependency"
strip_ansi "$work/ocaml.err" >"$work/ocaml.err.plain"
if ! grep -F \
    "resolved package identity 'different-name' does not match the requested dependency name" \
    "$work/ocaml.err.plain" >/dev/null; then
  echo "build-mismatched-dependency-name: OCaml did not reject the conflicting identity" >&2
  cat "$work/ocaml.out" "$work/ocaml.err" >&2
  exit 1
fi
run_case build-missing-dependency exit2 exit2 build "$work/missing-dependency"
require_both_errors_contain build-missing-dependency \
  "Could not build package tree reading dependency 'absent' at path '$work/missing-dependency'. Error:"
run_case build-configless-dependency exit2 exit2 build "$work/configless-dependency"
require_both_errors_contain build-configless-dependency \
  "Could not build package tree for 'no-config' at path '$work/configless-dependency'. Error:"
run_case build-malformed-dependency exit2 exit2 build "$work/malformed-dependency"
require_both_errors_contain build-malformed-dependency \
  "Could not build package tree for 'bad-config' at path '$work/malformed-dependency'. Error:"
run_case build-duplicate-dependency accept accept build "$work/duplicate-dependency"
duplicate_warning='Duplicated package: shared ./node_modules/shared (chosen) vs ./node_modules/a/node_modules/shared in ./node_modules/a'
normalize_project_path "$work/rust.err" "$work/rust.err.norm" \
  "$work/duplicate-dependency"
normalize_project_path "$work/ocaml.err" "$work/ocaml.err.norm" \
  "$work/duplicate-dependency"
if ! grep -F "$duplicate_warning" "$work/rust.err.norm" >/dev/null || \
  ! grep -F "$duplicate_warning" "$work/ocaml.err.norm" >/dev/null; then
  echo "Duplicate dependency warning was not emitted by both implementations" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.out" "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.out" "$work/ocaml.err" >&2
  exit 1
fi
run_cwd_case format-duplicate-dependency accept accept \
  "$work/duplicate-dependency" format
normalize_project_path "$work/rust.err" "$work/rust.err.norm" \
  "$work/duplicate-dependency"
normalize_project_path "$work/ocaml.err" "$work/ocaml.err.norm" \
  "$work/duplicate-dependency"
if ! grep -F "$duplicate_warning" "$work/rust.err.norm" >/dev/null || \
  ! grep -F "$duplicate_warning" "$work/ocaml.err.norm" >/dev/null; then
  echo "Format duplicate dependency warning was not emitted by both implementations" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.out" "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.out" "$work/ocaml.err" >&2
  exit 1
fi

set +e
REWATCH_REAL_BSC="$RESCRIPT_BSC_EXE" \
REWATCH_BSC_PROXY_MODE=delete-source \
REWATCH_SOURCE_TO_DELETE="$(command_path "$work/publication-race-rust/src/A.res")" \
REWATCH_SOURCE_DELETED="$(command_path "$work/publication-race-rust/source-deleted")" \
RESCRIPT_BSC_EXE="$delete_source_bsc" \
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
REWATCH_BSC_PROXY_MODE=delete-source \
REWATCH_SOURCE_TO_DELETE="$(command_path "$work/publication-race-ocaml/src/A.res")" \
REWATCH_SOURCE_DELETED="$(command_path "$work/publication-race-ocaml/source-deleted")" \
RESCRIPT_BSC_EXE="$delete_source_bsc" \
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
RAYON_NUM_THREADS=1 \
REWATCH_REAL_BSC="$RESCRIPT_BSC_EXE" \
REWATCH_BSC_PROXY_MODE=delete-parse-sources \
REWATCH_SOURCE_A="$(command_path "$work/parse-source-race-rust/src/A.res")" \
REWATCH_SOURCE_B="$(command_path "$work/parse-source-race-rust/src/B.res")" \
REWATCH_SOURCES_DELETED="$(command_path "$work/parse-source-race-rust/sources-deleted")" \
RESCRIPT_BSC_EXE="$delete_parse_sources_bsc" \
  "$rust" build "$work/parse-source-race-rust" \
  >"$work/rust.out" 2>"$work/rust.err"
rust_status=$?
REWATCH_REAL_BSC="$RESCRIPT_BSC_EXE" \
REWATCH_BSC_PROXY_MODE=delete-parse-sources \
REWATCH_SOURCE_A="$(command_path "$work/parse-source-race-ocaml/src/A.res")" \
REWATCH_SOURCE_B="$(command_path "$work/parse-source-race-ocaml/src/B.res")" \
REWATCH_SOURCES_DELETED="$(command_path "$work/parse-source-race-ocaml/sources-deleted")" \
RESCRIPT_BSC_EXE="$delete_parse_sources_bsc" \
  "$ocaml" build "$work/parse-source-race-ocaml" \
  >"$work/ocaml.out" 2>"$work/ocaml.err"
ocaml_status=$?
set -e
if [ "$rust_status" -ne 101 ] || \
  ! grep -F "file not found" "$work/rust.err" >/dev/null || \
  [ "$(classify "$ocaml_status")" != reject ] || \
  ! grep -F "parse-source-race" "$work/ocaml.err" >/dev/null || \
  ! grep -E 'A\.(res|ast)' "$work/ocaml.err" >/dev/null; then
  printf 'build-source-disappears-before-parse-read: expected Rust=panic and OCaml=path-bearing rejection, got Rust=%s/OCaml=%s\n' \
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
REWATCH_BSC_PROXY_MODE=delete-ast \
REWATCH_AST_DELETED="$(command_path "$work/ast-race-rust/ast-deleted")" \
RESCRIPT_BSC_EXE="$delete_ast_bsc" \
  "$rust" build "$work/ast-race-rust" \
  >"$work/rust.out" 2>"$work/rust.err"
rust_status=$?
REWATCH_REAL_BSC="$RESCRIPT_BSC_EXE" \
REWATCH_BSC_PROXY_MODE=delete-ast \
REWATCH_AST_DELETED="$(command_path "$work/ast-race-ocaml/ast-deleted")" \
RESCRIPT_BSC_EXE="$delete_ast_bsc" \
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
printf '{"name":"watch-config","sources":["src"],"dependencies":["definitely-missing-dep"]}\n' \
  >"$work/watch-config-ocaml/rescript.json"
wait_for_text "$work/watch-ocaml.err" "definitely-missing-dep"
if ! kill -0 "$ocaml_watch_pid" 2>/dev/null; then
  echo "OCaml watcher exited after a recoverable dependency error" >&2
  cat "$work/watch-ocaml.out" "$work/watch-ocaml.err" >&2
  exit 1
fi
printf '{"name":"watch-config","sources":["src"],"package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}\n' \
  >"$work/watch-config-ocaml/rescript.json"
wait_for_file "$work/watch-config-ocaml/src/A.mjs"
wait_for_file "$work/watch-config-ocaml/lib/bs/build.ninja"
if ! terminate_and_wait "$ocaml_watch_pid" "recoverable-config watcher"; then
  cat "$work/watch-ocaml.out" "$work/watch-ocaml.err" >&2
  exit 1
fi
checked=$((checked + 1))

retained_graph_marker="$work/watch-retained-graph/after-build.log"
REWATCH_WATCH_FILTER_MARKER="$retained_graph_marker" \
  "$ocaml" watch --after-build "node $command_work/watch-filter-marker.js" \
    "$work/watch-retained-graph" \
    >"$work/watch-retained-graph.out" \
    2>"$work/watch-retained-graph.err" &
retained_graph_pid=$!
background_pids="$background_pids $retained_graph_pid"
wait_for_line_count "$retained_graph_marker" 1
printf 'let newValue = 2\n' >"$work/watch-retained-graph/src/C.res"
printf 'let value = C.newValue\n' >"$work/watch-retained-graph/src/B.res"
wait_for_line_count "$retained_graph_marker" 2
wait_for_text "$work/watch-retained-graph/src/B.js" "C.newValue"
printf 'let value = C.newValue\n' >"$work/watch-retained-graph/src/B.res"
printf 'let newValue = B.value\n' >"$work/watch-retained-graph/src/C.res"
wait_for_text "$work/watch-retained-graph.err" \
  "Found a circular dependency in your code"
if ! kill -0 "$retained_graph_pid" 2>/dev/null; then
  echo "OCaml watcher exited after an incremental dependency cycle" >&2
  exit 1
fi
printf 'let newValue = 3\n' >"$work/watch-retained-graph/src/C.res"
wait_for_line_count "$retained_graph_marker" 3
terminate_and_wait "$retained_graph_pid" "retained-graph watcher"
checked=$((checked + 1))

"$ocaml" watch "$work/watch-dependency-recovery" \
  >"$work/watch-dependency-recovery.out" \
  2>"$work/watch-dependency-recovery.err" &
dependency_recovery_pid=$!
background_pids="$background_pids $dependency_recovery_pid"
wait_for_text "$work/watch-dependency-recovery.err" \
  "Could not build package tree for 'dep'"
if ! kill -0 "$dependency_recovery_pid" 2>/dev/null; then
  echo "OCaml watcher exited after a malformed dependency config" >&2
  exit 1
fi
printf '{"name":"dep","sources":["src"]}\n' \
  >"$work/watch-dependency-recovery/packages/dep/rescript.json"
wait_for_file "$work/watch-dependency-recovery/src/A.js"
terminate_and_wait "$dependency_recovery_pid" "dependency-recovery watcher"
checked=$((checked + 1))

"$ocaml" watch "$work/watch-dependency-install" \
  >"$work/watch-dependency-install.out" \
  2>"$work/watch-dependency-install.err" &
dependency_install_pid=$!
background_pids="$background_pids $dependency_install_pid"
if ! wait_for_text "$work/watch-dependency-install.err" \
    "Could not resolve dependency @scope/dep"; then
  printf '%s\n' '--- watcher stdout ---' >&2
  cat "$work/watch-dependency-install.out" >&2
  exit 1
fi
if ! kill -0 "$dependency_install_pid" 2>/dev/null; then
  echo "OCaml watcher exited while waiting for a missing dependency" >&2
  exit 1
fi
mkdir "$work/watch-dependency-install/node_modules/@scope"
sleep 1
mkdir -p "$work/watch-dependency-install/node_modules/@scope/dep/src"
printf '{"name":"@scope/dep","sources":["src"]}\n' \
  >"$work/watch-dependency-install/node_modules/@scope/dep/rescript.json"
printf 'let dependency = 1\n' \
  >"$work/watch-dependency-install/node_modules/@scope/dep/src/Dep.res"
wait_for_file "$work/watch-dependency-install/src/A.js"
terminate_and_wait "$dependency_install_pid" "dependency-install watcher"
checked=$((checked + 1))

directory_link "$work/watch-dependency-fallback/packages/dep" \
  "$work/node_modules/dep"
"$ocaml" watch "$work/watch-dependency-fallback" \
  >"$work/watch-dependency-fallback.out" \
  2>"$work/watch-dependency-fallback.err" &
dependency_fallback_pid=$!
background_pids="$background_pids $dependency_fallback_pid"
wait_for_text "$work/watch-dependency-fallback.err" \
  "no rescript.json or bsconfig.json"
mv "$work/watch-dependency-fallback/node_modules/dep" \
  "$work/watch-dependency-fallback/node_modules/dep-disabled"
wait_for_file "$work/watch-dependency-fallback/src/A.js"
terminate_and_wait "$dependency_fallback_pid" "dependency-fallback watcher"
checked=$((checked + 1))

if $file_symlinks_supported; then
  symlink_target_marker="$work/watch-symlink-target/after-build.log"
  REWATCH_WATCH_FILTER_MARKER="$symlink_target_marker" \
    "$ocaml" watch --after-build "node $command_work/watch-filter-marker.js" \
    "$work/watch-symlink-target" \
    >"$work/watch-symlink-target.out" 2>"$work/watch-symlink-target.err" &
  symlink_target_pid=$!
  background_pids="$background_pids $symlink_target_pid"
  wait_for_file "$work/watch-symlink-target/src/Linked.js"
  wait_for_line_count "$symlink_target_marker" 1
  printf 'let linked = 9876\n' \
    >"$work/watch-symlink-external/sub/Linked.res.next"
  mv "$work/watch-symlink-external/sub/Linked.res.next" \
    "$work/watch-symlink-external/sub/Linked.res"
  wait_for_text "$work/watch-symlink-target/src/Linked.js" "9876"
  wait_for_line_count "$symlink_target_marker" 2
  mv "$work/watch-symlink-external/sub/Linked.res" \
    "$work/watch-symlink-external/sub/Linked.res.removed"
  wait_for_line_count "$symlink_target_marker" 3
  if [ -e "$work/watch-symlink-target/src/Linked.js" ]; then
    echo "OCaml watcher retained output for a dangling source symlink" >&2
    exit 1
  fi
  mv "$work/watch-symlink-external/sub/Linked.res.removed" \
    "$work/watch-symlink-external/sub/Linked.res"
  wait_for_file "$work/watch-symlink-target/src/Linked.js"
  wait_for_line_count "$symlink_target_marker" 4
  mv "$work/watch-symlink-external/sub" \
    "$work/watch-symlink-external/sub.removed"
  wait_for_line_count "$symlink_target_marker" 5
  if [ -e "$work/watch-symlink-target/src/Linked.js" ]; then
    echo "OCaml watcher retained output after a symlink target parent moved" >&2
    exit 1
  fi
  mv "$work/watch-symlink-external/sub.removed" \
    "$work/watch-symlink-external/sub"
  wait_for_file "$work/watch-symlink-target/src/Linked.js"
  wait_for_line_count "$symlink_target_marker" 6
  terminate_and_wait "$symlink_target_pid" "symlink-target watcher"
  checked=$((checked + 1))
fi

feature_scope_marker="$work/watch-feature-scope/after-build.log"
scope_block_request="$work/watch-feature-scope/block-request"
scope_block_started="$work/watch-feature-scope/block-started"
scope_block_release="$work/watch-feature-scope/block-release"
REWATCH_SCOPE_REAL_BSC="$RESCRIPT_BSC_EXE" \
REWATCH_REAL_BSC="$RESCRIPT_BSC_EXE" \
REWATCH_BSC_PROXY_MODE=scope-block \
REWATCH_SCOPE_BLOCK_REQUEST="$(command_path "$scope_block_request")" \
REWATCH_SCOPE_BLOCK_STARTED="$(command_path "$scope_block_started")" \
REWATCH_SCOPE_BLOCK_RELEASE="$(command_path "$scope_block_release")" \
RESCRIPT_BSC_EXE="$watch_scope_bsc" \
REWATCH_WATCH_FILTER_MARKER="$feature_scope_marker" \
  "$ocaml" watch --features other \
    --after-build "node $command_work/watch-filter-marker.js" \
    "$work/watch-feature-scope" \
    >"$work/watch-feature-scope.out" 2>"$work/watch-feature-scope.err" &
feature_scope_pid=$!
background_pids="$background_pids $feature_scope_pid"
if ! wait_for_line_count "$feature_scope_marker" 1; then
  echo "OCaml feature-scope watcher did not finish its initial build" >&2
  cat "$work/watch-feature-scope.out" "$work/watch-feature-scope.err" >&2
  exit 1
fi
: >"$scope_block_request"
printf '{"name":"watch-feature-scope","sources":["src",{"dir":"inactive","feature":"inactive"}],"features":{"other":["inactive"]}}\n' \
  >"$work/watch-feature-scope/rescript.json"
wait_for_file "$scope_block_started"
printf 'let createdDuringBuild = 2\n' \
  >"$work/watch-feature-scope/inactive/CreatedDuringBuild.res"
: >"$scope_block_release"
wait_for_file "$work/watch-feature-scope/inactive/Inactive.js"
wait_for_file "$work/watch-feature-scope/inactive/CreatedDuringBuild.js"
wait_for_line_count "$feature_scope_marker" 3
if ! line_count_stays "$feature_scope_marker" 3; then
  echo "OCaml watcher lost or duplicated an edit during a source-scope transition" >&2
  cat "$work/watch-feature-scope.out" "$work/watch-feature-scope.err" >&2
  exit 1
fi
terminate_and_wait "$feature_scope_pid" "feature-scope watcher"
checked=$((checked + 1))

rust_filter_marker="$work/watch-filter-rust/after-build.log"
REWATCH_WATCH_FILTER_MARKER="$rust_filter_marker" \
  "$rust" watch --features other --filter 'Include\.res$' \
    --after-build "node $command_work/watch-filter-marker.js" \
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
terminate_and_wait "$rust_filter_pid" "Rust filter watcher"

ocaml_filter_marker="$work/watch-filter-ocaml/after-build.log"
REWATCH_WATCH_FILTER_MARKER="$ocaml_filter_marker" \
  "$ocaml" watch --features other --filter 'Include\.res$' \
    --after-build "node $command_work/watch-filter-marker.js" \
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
printf 'let value = 11\n' >"$work/watch-filter-ocaml/src/Exclude.res"
if ! line_count_stays "$ocaml_filter_marker" 2; then
  echo "OCaml watch filter rebuilt for an excluded-only edit" >&2
  cat "$work/watch-filter-ocaml.out" "$work/watch-filter-ocaml.err" >&2
  exit 1
fi
printf 'let value = 21\n' \
  >"$work/watch-filter-ocaml/inactive/Inactive.res"
if ! line_count_stays "$ocaml_filter_marker" 2; then
  echo "OCaml watcher rebuilt for a feature-disabled source edit" >&2
  cat "$work/watch-filter-ocaml.out" "$work/watch-filter-ocaml.err" >&2
  exit 1
fi
terminate_and_wait "$ocaml_filter_pid" "OCaml filter watcher"
checked=$((checked + 1))

for implementation in rust ocaml; do
  if [ "$implementation" = rust ]; then
    executable=$rust
  else
    executable=$ocaml
  fi
  quiet_watch_project="$work/quiet-watch-$implementation"
  quiet_watch_marker="$quiet_watch_project/after-build.log"
  REWATCH_WATCH_FILTER_MARKER="$quiet_watch_marker" \
    "$executable" -q watch \
      --after-build "node $command_work/watch-filter-marker.js" \
      "$quiet_watch_project" \
      >"$work/quiet-watch-$implementation.out" \
      2>"$work/quiet-watch-$implementation.err" &
  quiet_watch_pid=$!
  background_pids="$background_pids $quiet_watch_pid"
  wait_for_line_count "$quiet_watch_marker" 1
  printf 'let value = 2\n' >"$quiet_watch_project/src/A.res"
  wait_for_line_count "$quiet_watch_marker" 2
  printf 'let value =\n' >"$quiet_watch_project/src/A.res"
  wait_for_text "$work/quiet-watch-$implementation.err" \
    "This let-binding misses an expression"
  if ! kill -0 "$quiet_watch_pid" 2>/dev/null; then
    echo "quiet-watch-$implementation: watcher exited after a parse error" >&2
    exit 1
  fi
  printf 'let value = 3\n' >"$quiet_watch_project/src/A.res"
  wait_for_line_count "$quiet_watch_marker" 3
  rm "$quiet_watch_project/lib/watch.lock"
  wait_for_exit "$quiet_watch_pid"
  wait "$quiet_watch_pid"
  if [ -s "$work/quiet-watch-$implementation.out" ] || \
    grep -F "Incremental build failed" \
      "$work/quiet-watch-$implementation.err" >/dev/null; then
    echo "quiet-watch-$implementation: quiet watch emitted progress or a duplicate failure summary" >&2
    cat "$work/quiet-watch-$implementation.out" \
      "$work/quiet-watch-$implementation.err" >&2
    exit 1
  fi
  normalize_project_path "$work/quiet-watch-$implementation.err" \
    "$work/quiet-watch-$implementation.err.norm" "$quiet_watch_project"
done
if ! cmp -s "$work/quiet-watch-rust.err.norm" \
  "$work/quiet-watch-ocaml.err.norm"; then
  echo "Quiet watch failure diagnostics differ" >&2
  printf '%s\n' '--- Rust stderr ---' >&2
  cat "$work/quiet-watch-rust.err.norm" >&2
  printf '%s\n' '--- OCaml stderr ---' >&2
  cat "$work/quiet-watch-ocaml.err.norm" >&2
  exit 1
fi
checked=$((checked + 1))

run_case clean-missing-dependency exit2 exit2 clean "$work/missing-dependency"
run_case clean-configless-dependency exit2 exit2 clean "$work/configless-dependency"
run_case clean-malformed-dependency exit2 exit2 clean "$work/malformed-dependency"
set +e
"$rust" clean "$work/clean-duplicate-rust" \
  >"$work/rust.out" 2>"$work/rust.err"
rust_status=$?
"$ocaml" clean "$work/clean-duplicate-ocaml" \
  >"$work/ocaml.out" 2>"$work/ocaml.err"
ocaml_status=$?
set -e
if [ "$(classify "$rust_status")" != reject ] || [ "$ocaml_status" -ne 0 ] || \
  [ ! -e "$work/clean-duplicate-rust/src/one/A.js" ] || \
  [ ! -e "$work/clean-duplicate-rust/src/two/A.js" ] || \
  [ -e "$work/clean-duplicate-rust/lib/bs/marker" ] || \
  [ -e "$work/clean-duplicate-ocaml/src/one/A.js" ] || \
  [ -e "$work/clean-duplicate-ocaml/src/two/A.js" ] || \
  [ -e "$work/clean-duplicate-ocaml/lib/bs/marker" ]; then
  echo "clean-duplicate-modules: cleanup behavior differs unexpectedly" >&2
  printf '%s\n' '--- Rust output ---' >&2
  cat "$work/rust.out" "$work/rust.err" >&2
  printf '%s\n' '--- OCaml output ---' >&2
  cat "$work/ocaml.out" "$work/ocaml.err" >&2
  exit 1
fi
checked=$((checked + 1))
set +e
"$rust" watch "$work/missing-dependency" >"$work/rust.out" 2>"$work/rust.err"
rust_status=$?
set -e
if [ "$rust_status" -ne 2 ]; then
  echo "watch-missing-dependency: expected Rust exit 2, got $rust_status" >&2
  cat "$work/rust.out" "$work/rust.err" >&2
  exit 1
fi
"$ocaml" watch "$work/missing-dependency" \
  >"$work/ocaml.out" 2>"$work/ocaml.err" &
missing_dependency_watch_pid=$!
background_pids="$background_pids $missing_dependency_watch_pid"
wait_for_text "$work/ocaml.err" "Could not resolve dependency absent"
if ! kill -0 "$missing_dependency_watch_pid" 2>/dev/null; then
  echo "OCaml watcher exited after its initial dependency error" >&2
  cat "$work/ocaml.out" "$work/ocaml.err" >&2
  exit 1
fi
printf '{"name":"missing-dependency","sources":["src"]}\n' \
  >"$work/missing-dependency/rescript.json"
wait_for_file "$work/missing-dependency/src/A.js"
terminate_and_wait "$missing_dependency_watch_pid" \
  "missing-dependency watcher"
if $windows_posix_shell; then
  # MSYS cannot deliver the graceful console event that lets a native watcher
  # remove its lock. Its external termination intentionally leaves a stale
  # watch lock, so remove that harness artifact before checking failure paths.
  rm -f "$work/missing-dependency/lib/watch.lock"
fi
printf '{"name":"missing-dependency","sources":["src"],"dependencies":["absent"]}\n' \
  >"$work/missing-dependency/rescript.json"
checked=$((checked + 1))
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
