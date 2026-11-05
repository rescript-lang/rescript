#!/bin/bash
# Checks command-line validation, diagnostics, and redirected output of the
# rescript executable. Outputs are compared with the snapshots in
# snapshots/command_validation; UPDATE_SNAPSHOTS=1 rewrites them.
set -eu
. "$(dirname "$0")/test_helpers.sh"
# CI forces colors; the snapshots hold redirected output, which has none.
# Cases that test forced colors set the variable themselves.
unset CLICOLOR_FORCE

root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
rescript=${1:-$root/_build/default/rewatch-ocaml/rescript_ocaml.exe}
rescript=$(realpath "$rescript")
work=$(mktemp -d "${TMPDIR:-/tmp}/rewatch-command-validation-XXXXXX")
# Compiler diagnostics contain canonical paths. Resolve platform aliases such
# as macOS's /var -> /private/var before deriving paths used for comparison.
work=$(realpath "$work")
native_work=$work
native_short_work=$work
if $windows_posix_shell; then
  native_work=$(cd "$work" && pwd -W)
  native_user=$(cygpath -w "$USERPROFILE" | tr '\\' '/')
  native_short_user=$(cygpath -w -s "$USERPROFILE" | tr '\\' '/')
  native_short_work=${native_work/"$native_user"/"$native_short_user"}
fi
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
command_work=$native_work
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
mkdir -p "$work/exotic-module/src"
mkdir -p "$work/filter-basename/src/nested"
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
mkdir -p "$work/clean-missing-runtime/src" \
  "$work/clean-missing-runtime/lib/bs"
mkdir -p "$work/clean-duplicate/src/one" \
  "$work/clean-duplicate/src/two" \
  "$work/clean-duplicate/lib/bs"
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
mkdir -p "$work/watch-config/src"
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
mkdir -p "$work/watch-filter/src" "$work/watch-filter/inactive"
mkdir -p "$work/quiet-watch/src"
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
printf '{"name":"exotic-module","namespace":"Ns","sources":["src"]}\n' \
  >"$work/exotic-module/rescript.json"
printf 'let value = 1\n' >"$work/exotic-module/src/Main.res"
printf 'let value = 2\n' >"$work/exotic-module/src/foo-bar.res"
printf '{"name":"filter-basename","sources":[{"dir":"src","subdirs":true}]}\n' \
  >"$work/filter-basename/rescript.json"
printf 'let value = 1\n' >"$work/filter-basename/src/nested/A.res"
printf 'let value = 2\n' >"$work/filter-basename/src/nested/B2.res"
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
printf '{"name":"clean-missing-runtime","sources":["src"]}\n' \
  >"$work/clean-missing-runtime/rescript.json"
printf 'let value = 1\n' >"$work/clean-missing-runtime/src/A.res"
printf 'owned compiler artifact\n' >"$work/clean-missing-runtime/lib/bs/marker"
printf '%s\n' \
  '{"name":"duplicate-clean","sources":{"dir":"src","subdirs":true},"package-specs":{"module":"esmodule","in-source":true}}' \
  >"$work/clean-duplicate/rescript.json"
printf 'let value = 1\n' >"$work/clean-duplicate/src/one/A.res"
printf 'let value = 2\n' >"$work/clean-duplicate/src/two/A.res"
printf 'generated\n' >"$work/clean-duplicate/src/one/A.js"
printf 'generated\n' >"$work/clean-duplicate/src/two/A.js"
printf 'owned compiler artifact\n' >"$work/clean-duplicate/lib/bs/marker"
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
printf '{"name":"watch-config","sources":["src"]}\n' \
  >"$work/watch-config/rescript.json"
printf 'let value = 1\n' >"$work/watch-config/src/A.res"
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
printf '{"name":"watch-filter","sources":["src",{"dir":"inactive","feature":"inactive"}]}\n' \
  >"$work/watch-filter/rescript.json"
printf 'let value = 1\n' >"$work/watch-filter/src/Include.res"
printf 'let value = 10\n' >"$work/watch-filter/src/Exclude.res"
printf 'let value = 20\n' >"$work/watch-filter/inactive/Inactive.res"
printf '{"name":"quiet-watch","sources":["src"]}\n' \
  >"$work/quiet-watch/rescript.json"
printf 'let value = 1\n' >"$work/quiet-watch/src/A.res"
printf 'require("fs").appendFileSync(process.env.REWATCH_WATCH_FILTER_MARKER, "done\\n")\n' \
  >"$work/watch-filter-marker.js"
default_runtime=$root/packages/@rescript/runtime
case $(uname -s) in
  MINGW*|MSYS*)
    default_runtime=$(cygpath -w "$default_runtime")
    ;;
esac
export RESCRIPT_RUNTIME=${RESCRIPT_RUNTIME:-$default_runtime}

classify() {
  case "$1" in
    0) printf accept ;;
    2) printf exit2 ;;
    *) printf reject ;;
  esac
}

strip_ansi() {
  LC_ALL=C sed $'s/\033\\[[0-9;]*m//g' "$1"
}

# normalize_project_path INPUT OUTPUT PROJECT: replaces every spelling of
# PROJECT with <ROOT>, for checks that grep for project-relative paths.
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
    text = text.replaceAll("\r\n", "\n");
    text = text.replaceAll("\\", "/");
    text = text.replaceAll("./<ROOT>/", "./");
    fs.writeFileSync(process.argv[2], text);
  ' "$input" "$output"
}

dump_output() {
  printf '%s\n' '--- stdout ---' >&2
  cat "$work/rescript.out" >&2
  printf '%s\n' '--- stderr ---' >&2
  cat "$work/rescript.err" >&2
}

# run_rescript [--cwd DIR] [--unset VAR] ARGS...
# Runs rescript with ARGS, writing $work/rescript.{out,err} and setting
# rescript_status.
run_rescript() {
  local cwd="" unset_variable=""
  local -a command
  while :; do
    case $1 in
      --cwd) cwd=$2; shift 2 ;;
      --unset) unset_variable=$2; shift 2 ;;
      *) break ;;
    esac
  done
  command=("$rescript" "$@")
  if [ -n "$unset_variable" ]; then
    command=(env -u "$unset_variable" "${command[@]}")
  fi
  set +e
  if [ -n "$cwd" ]; then
    (cd "$cwd" && "${command[@]}") \
      >"$work/rescript.out" 2>"$work/rescript.err"
  else
    "${command[@]}" >"$work/rescript.out" 2>"$work/rescript.err"
  fi
  rescript_status=$?
  set -e
}

checked=0
# run_case [--cwd DIR] NAME EXPECTED ARGS...
# EXPECTED is accept (exit 0), exit2 (usage error), or reject (other failure).
run_case() {
  local -a options=()
  if [ "$1" = --cwd ]; then
    options=(--cwd "$2")
    shift 2
  fi
  name=$1
  expected=$2
  shift 2
  run_rescript ${options[@]+"${options[@]}"} "$@"
  if [ "$(classify "$rescript_status")" != "$expected" ]; then
    printf '%s: expected %s, got status %s\n' \
      "$name" "$expected" "$rescript_status" >&2
    dump_output
    exit 1
  fi
  checked=$((checked + 1))
}

# check_output_snapshot [--stderr-only] NAME [NAME=PATH...]: compares the
# status, stdout, and stderr of the last run with
# snapshots/command_validation/NAME.txt. The temporary directory becomes <WORK>
# and the runtime <RUNTIME>. --stderr-only leaves out stdout, for commands that
# report packages in an unspecified order.
check_output_snapshot() {
  local include_stdout=true
  if [ "$1" = --stderr-only ]; then
    include_stdout=false
    shift
  fi
  local name=$1
  shift
  {
    printf 'status: %s\n' "$rescript_status"
    if $include_stdout; then
      printf '%s\n' '--- stdout'
      cat "$work/rescript.out"
    fi
    printf '%s\n' '--- stderr'
    cat "$work/rescript.err"
  } >"$work/$name.snapshot"
  if ! check_snapshot "command_validation/$name.txt" "$work/$name.snapshot" \
    "WORK=$work" "RUNTIME=$RESCRIPT_RUNTIME" "$@"; then
    exit 1
  fi
}

require_error_contains() {
  local name=$1
  local fragment=$2
  local native_fragment=${fragment/"$work"/"$native_work"}
  local native_short_fragment=${fragment/"$work"/"$native_short_work"}
  if ! { grep -F "$fragment" "$work/rescript.err" >/dev/null || \
      tr '\\' '/' <"$work/rescript.err" | grep -F "$native_fragment" >/dev/null || \
      tr '\\' '/' <"$work/rescript.err" | grep -F "$native_short_fragment" >/dev/null; }; then
    printf '%s: expected the error to contain %s\n' "$name" "$fragment" >&2
    dump_output
    exit 1
  fi
}

# run_build_output_case NAME STATUS FIXTURE MODE: builds a copy of FIXTURE with
# redirected output (MODE redirected, quiet, or forced-color) and compares the
# output with the case's snapshot.
run_build_output_case() {
  name=$1
  expected_status=$2
  fixture=$3
  mode=$4
  build_project="$work/$name-build"
  cp -R "$fixture" "$build_project"
  if [ "$mode" = quiet ]; then
    run_rescript -q build "$build_project"
  elif [ "$mode" = forced-color ]; then
    CLICOLOR_FORCE=1 run_rescript build "$build_project"
  else
    run_rescript build "$build_project"
  fi
  if [ "$rescript_status" -ne "$expected_status" ]; then
    printf '%s: %s build exited with %s, expected %s\n' \
      "$name" "$mode" "$rescript_status" "$expected_status" >&2
    dump_output
    exit 1
  fi
  check_output_snapshot "$name" "ROOT=$build_project"
  checked=$((checked + 1))
}

run_multiple_parse_errors_case() {
  local build_project="$work/multiple-parse-errors-build"
  cp -R "$work/multiple-parse-errors" "$build_project"
  run_rescript build "$build_project"
  if [ "$rescript_status" -ne 1 ]; then
    printf 'multiple-parse-errors: expected status 1, got %s\n' \
      "$rescript_status" >&2
    exit 1
  fi
  error="$work/rescript.err"
  if [ "$(grep -cF 'Error in multiple-parse-errors:' "$error")" -ne 2 ] || \
    ! grep -E '[/\\]src[/\\]A\.res' "$error" >/dev/null || \
    ! grep -E '[/\\]src[/\\]B\.res' "$error" >/dev/null; then
    echo "rescript did not retain every independent parse error" >&2
    cat "$error" >&2
    exit 1
  fi
  checked=$((checked + 1))
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
if [ -s "$work/rescript.out" ] || [ -s "$work/rescript.err" ]; then
  echo "quiet-success: a clean build emitted output" >&2
  dump_output
  exit 1
fi
run_build_output_case quiet-compile-error 1 \
  "$root/rewatch-ocaml/tests/failure" quiet
run_build_output_case quiet-parse-error 1 "$work/redirected-parse-fixture" quiet
run_build_output_case quiet-warning 0 \
  "$root/rewatch-ocaml/tests/warning-replay" quiet

run_case build-subcommand-version accept build --version
run_case clustered-global-version accept -vV build
run_case clustered-global-verbosity accept -vvvvv build "$project"
run_case clustered-global-version-before-help accept -Vh build
run_case clustered-global-help-before-version accept -hV build
run_case clustered-subcommand-help-before-version accept build -hV
run_case clustered-subcommand-version-before-help accept build -Vh
"$rescript" build --help=groff >"$work/help-groff.out"
if ! grep -F '.\" Pipe this output to groff' \
  "$work/help-groff.out" >/dev/null; then
  echo "formatted-help: the requested groff format was not preserved" >&2
  cat "$work/help-groff.out" >&2
  exit 1
fi
checked=$((checked + 1))
run_case implicit-conflicting-verbosity exit2 -v -q
run_case build-conflicting-verbosity exit2 build --verbose --quiet
run_case filter-perl-quoting exit2 build --filter '\QFoo.res\E' "$project"
run_case filter-comment-group exit2 build --filter '(?#note)Foo' "$project"
run_case filter-octal-escape exit2 build --filter '\123' "$project"
run_case filter-braced-octal-escape exit2 build --filter '\o{123}' "$project"
run_case filter-perl-control-escape-e exit2 build --filter '\e' "$project"
run_case filter-perl-anchor-g exit2 build --filter '\G' "$project"
run_case filter-perl-anchor-z exit2 build --filter '\Z' "$project"
run_case filter-descending-range exit2 build --filter '[z-a]' "$project"
run_case filter-escaped-range-start exit2 build --filter '[\d-z]' "$project"
run_case filter-in-class-nonword exit2 build --filter \
  '^[\W]+\.res$' "$project"
run_case filter-collating-element exit2 build --filter '[[.a.]]' "$project"
run_case filter-class-algebra exit2 build --filter \
  '[a-z&&[^aeiou]]' "$project"
run_case build-no-timing-preserves-folder accept build --no-timing "$project"
run_case compiler-args-source accept compiler-args "$project/src/A.res"
run_case compiler-args-extension reject compiler-args "$project/src/A.txt"
run_case compiler-args-missing reject compiler-args "$project/src/Missing.res"
run_case compiler-args-no-project reject compiler-args "$work/orphan/A.res"

run_rescript --unset RESCRIPT_RUNTIME clean "$work/clean-missing-runtime"
if [ "$rescript_status" -ne 0 ] || \
  [ -e "$work/clean-missing-runtime/lib/bs/marker" ]; then
  echo "clean-missing-runtime: expected clean to succeed without a runtime" >&2
  dump_output
  exit 1
fi
checked=$((checked + 1))
run_rescript --unset RESCRIPT_RUNTIME build "$work/missing-runtime-package"
if [ "$rescript_status" -eq 0 ]; then
  echo "build-missing-runtime-package: expected the build to reject" >&2
  exit 1
fi
require_error_contains build-missing-runtime-package \
  'The rescript runtime package could not be found.'
require_error_contains build-missing-runtime-package \
  'Please set RESCRIPT_RUNTIME environment variable'
checked=$((checked + 1))
RESCRIPT_RUNTIME="$work/missing-runtime" run_rescript build "$project"
if [ "$rescript_status" -eq 0 ] || \
  ! grep -F "RESCRIPT_RUNTIME points to missing path" \
    "$work/rescript.err" >/dev/null || \
  ! grep -F "missing-runtime" "$work/rescript.err" >/dev/null; then
  echo "build-stale-runtime: expected a contextual preflight rejection" >&2
  dump_output
  exit 1
fi
checked=$((checked + 1))
run_case format-missing-file reject format "$project/src/Missing.res"
check_output_snapshot format-missing-file
run_case format-directory reject format "$project/src"
check_output_snapshot format-directory
run_case format-unsupported-extension reject format "$project/src/A.txt"
check_output_snapshot format-unsupported-extension
run_case --cwd "$work/empty" format-no-config reject format
require_error_contains format-no-config \
  "Could not read rescript.json at $work/empty:"
require_error_contains format-no-config "$work/empty/bsconfig.json"
run_case --cwd "$work/malformed" format-malformed-config reject format
require_error_contains format-malformed-config \
  "Could not read rescript.json at $work/malformed:"
if ! grep -F 'invalid JSON' "$work/rescript.err" >/dev/null; then
  echo "format-malformed-config: JSON parser context was lost" >&2
  dump_output
  exit 1
fi
run_case --cwd "$work/config-directory" \
  format-config-directory reject format
require_error_contains format-config-directory \
  "$work/config-directory/rescript.json"
if ! grep -F 'Is a directory' "$work/rescript.err" >/dev/null; then
  echo "format-config-directory: file-kind diagnostic was lost" >&2
  exit 1
fi
set +e
printf 'let value =\n' | "$rescript" format --stdin .res \
  >"$work/rescript.out" 2>"$work/rescript.err"
rescript_status=$?
set -e
if [ "$rescript_status" -eq 0 ] || [ -s "$work/rescript.out" ]; then
  echo "format-invalid-stdin: expected the formatter to reject" >&2
  exit 1
fi
require_error_contains format-invalid-stdin 'Error formatting stdin:'
require_error_contains format-invalid-stdin \
  'This let-binding misses an expression'
checked=$((checked + 1))
mkdir -p "$work/format-write"
printf 'let value=1\n' >"$work/format-write/A.res"
chmod 0444 "$work/format-write/A.res"
if [ ! -w "$work/format-write/A.res" ]; then
  run_rescript format "$work/format-write/A.res"
  if [ "$rescript_status" -eq 0 ] || \
    ! grep -F "Could not write formatted file" "$work/rescript.err" >/dev/null || \
    ! grep -F "format-write/A.res" \
      < <(tr '\\' '/' <"$work/rescript.err") >/dev/null; then
    echo "format-write-failure: formatter write failures lost context" >&2
    dump_output
    exit 1
  fi
  checked=$((checked + 1))
fi
chmod 0644 "$work/format-write/A.res"

run_case build-missing-folder reject build "$work/missing"
run_case build-existing-folder-without-config reject build "$work/empty"
require_error_contains build-existing-folder-without-config "$work/empty"
run_case build-malformed-config reject build "$work/malformed"
require_error_contains build-malformed-config \
  "$work/malformed"
run_case build-malformed-parent reject build "$work/malformed-parent/child"
require_error_contains build-malformed-parent \
  "$work/malformed-parent"
run_case build-config-path-is-directory reject build "$work/config-directory"
require_error_contains build-config-path-is-directory \
  "$work/config-directory"
run_case after-build-nonzero-is-not-ignored reject build --after-build \
  "node $command_work/failing-after-build.js" "$project"
if ! grep -F -- '--after-build command failed with exit code 7' \
    "$work/rescript.err" >/dev/null || \
  ! grep -F 'hook failed' "$work/rescript.err" >/dev/null; then
  echo "A nonzero --after-build exit was not reported" >&2
  dump_output
  exit 1
fi
run_case after-build-empty reject build --after-build '' "$project"
if ! grep -F -- '--after-build command cannot be empty' \
  "$work/rescript.err" >/dev/null; then
  echo "Empty --after-build did not produce a contextual error" >&2
  dump_output
  exit 1
fi
run_case after-build-missing-program reject build --after-build \
  rewatch-command-that-does-not-exist "$project"
if ! grep -F 'Could not run --after-build command' \
    "$work/rescript.err" >/dev/null || \
  ! grep -F 'rewatch-command-that-does-not-exist' \
    "$work/rescript.err" >/dev/null; then
  echo "Missing --after-build program did not produce a contextual error" >&2
  dump_output
  exit 1
fi
run_case js-post-build-nonzero reject build "$work/failed-js-post-build"
js_post_build_error="js-post-build command failed for $work/failed-js-post-build/src/A.js"
require_error_contains js-post-build-nonzero "$js_post_build_error"
printf 'not-a-pid' >"$work/malformed-lock/lib/build.lock"
run_case build-malformed-lock reject build "$work/malformed-lock"
if [ "$(cat "$work/malformed-lock/lib/build.lock")" != not-a-pid ]; then
  echo "A malformed build lock with unknown ownership was replaced" >&2
  exit 1
fi
if [ -n "$(find "$work/malformed-lock/lib" -maxdepth 1 \
  -name '.build-lock-*.tmp' -print -quit)" ]; then
  echo "A build-lock candidate was left after acquisition failed" >&2
  exit 1
fi
# Use the real executable as the lock owner. Copying an MSYS utility to a
# rescript-prefixed name does not change the image name reported by tasklist,
# and locks owned by unrelated executables are deliberately rejected to
# protect against PID reuse.
"$rescript" watch "$work/signal-lock-owner" \
  >"$work/signal-lock-owner.out" 2>"$work/signal-lock-owner.err" &
signal_lock_owner_pid=$!
background_pids="$background_pids $signal_lock_owner_pid"
wait_for_text "$work/signal-lock-owner.out" "Finished initial compilation"
printf '%s' "$(lock_owner_pid "$signal_lock_owner_pid")" \
  >"$work/signal-lock/lib/build.lock"
"$rescript" watch "$work/signal-lock" \
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
# so no Windows console control event reaches rescript and the status is
# nonzero. The native Windows unit tests cover deferred signal dispatch; this
# integration case still proves that terminating a waiter cannot emit a
# spurious diagnostic.
if [ -s "$work/signal-lock.err" ]; then
  echo "Signal during build-lock wait emitted a diagnostic" >&2
  cat "$work/signal-lock.err" >&2
  exit 1
fi
kill -TERM "$signal_lock_owner_pid"
wait "$signal_lock_owner_pid" 2>/dev/null || true
checked=$((checked + 1))
printf 'not-a-pid' >"$work/malformed-lock/lib/watch.lock"
run_case watch-malformed-lock reject watch "$work/malformed-lock"
if [ "$(cat "$work/malformed-lock/lib/watch.lock")" != not-a-pid ]; then
  echo "A malformed watch lock with unknown ownership was replaced" >&2
  exit 1
fi
if [ -n "$(find "$work/malformed-lock/lib" -maxdepth 1 \
  -name '.watch-lock-*.tmp' -print -quit)" ]; then
  echo "A watch-lock candidate was left after acquisition failed" >&2
  exit 1
fi
printf '{ invalid json\n' >"$work/watch-lock-order/rescript.json"
# A copied MSYS utility keeps its original Windows image name and is therefore
# rejected as an unrelated lock owner. Run rescript as a real watcher so both
# process identity and the native PID match production.
owner_dir="$work/watch-lock-owner"
mkdir -p "$owner_dir/src"
printf '{"name":"watch-lock-owner","sources":["src"]}\n' \
  >"$owner_dir/rescript.json"
printf 'let value = 1\n' >"$owner_dir/src/A.res"
"$rescript" watch "$owner_dir" \
  >"$owner_dir/watch.out" 2>"$owner_dir/watch.err" &
watch_lock_owner_pid=$!
background_pids="$background_pids $watch_lock_owner_pid"
wait_for_text "$owner_dir/watch.out" "Finished initial compilation"
native_owner_pid=$(lock_owner_pid "$watch_lock_owner_pid")
if [ -z "$native_owner_pid" ]; then
  echo "watch-lock-before-config: could not resolve the owner PID" >&2
  exit 1
fi
printf '%s' "$native_owner_pid" \
  >"$work/watch-lock-order/lib/watch.lock"
run_rescript watch "$work/watch-lock-order"
if [ "$(classify "$rescript_status")" != reject ] || \
  ! grep -F 'A ReScript build is already running' \
    "$work/rescript.err" >/dev/null || \
  grep -F 'invalid JSON' "$work/rescript.err" >/dev/null; then
  echo "watch-lock-before-config: lock acquisition did not precede config parsing" >&2
  dump_output
  exit 1
fi
kill -TERM "$watch_lock_owner_pid"
wait "$watch_lock_owner_pid" 2>/dev/null || true
checked=$((checked + 1))
run_case build-interface-path-mismatch reject build \
  "$work/interface-mismatch"
check_output_snapshot --stderr-only build-interface-path-mismatch
run_rescript build "$work/exotic-module"
mlmap="$work/exotic-module/lib/bs/Ns.mlmap"
if [ "$(classify "$rescript_status")" != accept ] || \
  ! grep -Fx Main "$mlmap" >/dev/null || \
  grep -F 'Foo-bar' "$mlmap" >/dev/null; then
  printf 'namespace-exotic-module: expected a successful build, got %s\n' \
    "$rescript_status" >&2
  dump_output
  cat "$mlmap" >&2
  exit 1
fi
if ! check_snapshot command_validation/namespace-exotic-module.mlmap "$mlmap"; then
  exit 1
fi
checked=$((checked + 1))
run_rescript build --filter nested "$work/filter-basename"
if [ "$(classify "$rescript_status")" != accept ] || \
  [ -e "$work/filter-basename/src/nested/A.js" ]; then
  echo "Source filters did not ignore directory-only matches" >&2
  dump_output
  exit 1
fi
checked=$((checked + 1))
run_rescript build --filter 'A\.res$' "$work/filter-basename"
if [ "$(classify "$rescript_status")" != accept ] || \
  [ ! -e "$work/filter-basename/src/nested/A.js" ]; then
  echo "Source filters did not include a basename match" >&2
  dump_output
  exit 1
fi
checked=$((checked + 1))
run_rescript build --filter '(?:A|B\d)\.res$' "$work/filter-basename"
if [ "$(classify "$rescript_status")" != accept ] || \
  [ ! -e "$work/filter-basename/src/nested/B2.js" ]; then
  echo "Source filters did not support non-capturing groups and \\d" >&2
  dump_output
  exit 1
fi
checked=$((checked + 1))
run_case build-excludes-external-dev-source accept build \
  "$work/external-dev-source"
run_case build-ignores-dormant-external-dev-permission accept build \
  "$work/external-dev-permission"
run_case build-reports-all-active-permission-failures reject build \
  "$work/active-permission"
strip_ansi "$work/rescript.out" >"$work/rescript.out.plain"
strip_ansi "$work/rescript.err" >"$work/rescript.err.plain"
if ! grep -Fx 'root dependencies: a' "$work/rescript.err.plain" >/dev/null || \
  ! grep -Fx 'root dependencies: b' "$work/rescript.err.plain" >/dev/null || \
  ! grep -F 'Update allowed-dependents in the dependency rescript.json files.' \
    "$work/rescript.err.plain" >/dev/null || [ -s "$work/rescript.out.plain" ]; then
  echo "Active dependency-permission diagnostics changed unexpectedly" >&2
  dump_output
  exit 1
fi
run_case build-source-path-is-file accept build "$work/source-path-file"
if ! grep -F 'Could not read folder: "src"' "$work/rescript.err" >/dev/null; then
  echo "Non-directory source diagnostic is missing" >&2
  dump_output
  exit 1
fi
check_output_snapshot --stderr-only build-source-path-is-file
run_case build-missing-source-folder accept build \
  "$work/missing-source-folder"
check_output_snapshot --stderr-only build-missing-source-folder
run_case build-dependency-without-sources accept build \
  "$work/dependency-without-sources"
check_output_snapshot --stderr-only build-dependency-without-sources
run_case build-default-feature-cycle accept build \
  "$work/default-feature-cycle"
run_case --cwd "$work/format-feature-cycle" \
  format-requested-feature-cycle reject format
check_output_snapshot --stderr-only format-requested-feature-cycle
run_case --cwd "$work/format-source-selection" \
  format-scans-graph-with-effective-features reject format --check
normalize_project_path "$work/rescript.err" "$work/rescript.err.norm" \
  "$work/format-source-selection"
missing_folder='Could not read folder: "missing". Specified in dependency: installed'
base_file='[format check] <ROOT>/packages/local/base/Base.res'
native_file='[format check] <ROOT>/packages/local/native/Native.res'
other_file='<ROOT>/packages/local/other/Other.res'
if ! grep -F "$missing_folder" "$work/rescript.err.norm" >/dev/null || \
  ! grep -F "$base_file" "$work/rescript.err.norm" >/dev/null || \
  ! grep -F "$native_file" "$work/rescript.err.norm" >/dev/null || \
  grep -F "$other_file" "$work/rescript.err.norm" >/dev/null || \
  ! grep -F 'The 2 files listed above need formatting' \
    "$work/rescript.err.norm" >/dev/null; then
  echo "Implicit format package scanning or feature selection changed" >&2
  dump_output
  exit 1
fi
run_case clean-dependency-without-sources accept clean \
  "$work/dependency-without-sources"
check_output_snapshot --stderr-only clean-dependency-without-sources
run_rescript clean "$project"
if [ "$rescript_status" -ne 0 ] || \
  ! grep -Fx 'Cleaning command-validation' "$work/rescript.out" >/dev/null; then
  echo "Redirected clean progress changed" >&2
  dump_output
  exit 1
fi
check_output_snapshot clean-redirected
checked=$((checked + 1))
run_rescript -q clean "$project"
if [ "$rescript_status" -ne 0 ] || \
  [ -s "$work/rescript.out" ] || [ -s "$work/rescript.err" ]; then
  echo "Quiet redirected clean emitted output" >&2
  dump_output
  exit 1
fi
checked=$((checked + 1))
run_case --cwd "$work/dependency-without-sources" \
  format-dependency-without-sources accept format
check_output_snapshot --stderr-only format-dependency-without-sources
run_case build-package-name-mismatch accept build \
  "$work/package-name-mismatch"
check_output_snapshot --stderr-only build-package-name-mismatch \
  "ROOT=$work/package-name-mismatch"
run_case build-malformed-package-json reject build \
  "$work/malformed-package-json"
require_error_contains build-malformed-package-json \
  'Could not initialize build: Could not parse package.json:'
run_case build-mismatched-dependency-name exit2 build \
  "$work/mismatched-dependency"
strip_ansi "$work/rescript.err" >"$work/rescript.err.plain"
if ! grep -F \
    "resolved package identity 'different-name' does not match the requested dependency name" \
    "$work/rescript.err.plain" >/dev/null; then
  echo "build-mismatched-dependency-name: the conflicting identity was not rejected" >&2
  dump_output
  exit 1
fi
run_case build-missing-dependency exit2 build "$work/missing-dependency"
require_error_contains build-missing-dependency \
  "Could not build package tree reading dependency 'absent' at path '$work/missing-dependency'. Error:"
run_case build-configless-dependency exit2 build "$work/configless-dependency"
require_error_contains build-configless-dependency \
  "Could not build package tree for 'no-config' at path '$work/configless-dependency'. Error:"
run_case build-malformed-dependency exit2 build "$work/malformed-dependency"
require_error_contains build-malformed-dependency \
  "Could not build package tree for 'bad-config' at path '$work/malformed-dependency'. Error:"
run_case build-duplicate-dependency accept build "$work/duplicate-dependency"
duplicate_warning='Duplicated package: shared ./node_modules/shared (chosen) vs ./node_modules/a/node_modules/shared in ./node_modules/a'
normalize_project_path "$work/rescript.err" "$work/rescript.err.norm" \
  "$work/duplicate-dependency"
if ! grep -F "$duplicate_warning" "$work/rescript.err.norm" >/dev/null; then
  echo "The duplicate dependency warning was not emitted" >&2
  dump_output
  exit 1
fi
run_case --cwd "$work/duplicate-dependency" \
  format-duplicate-dependency accept format
normalize_project_path "$work/rescript.err" "$work/rescript.err.norm" \
  "$work/duplicate-dependency"
if ! grep -F "$duplicate_warning" "$work/rescript.err.norm" >/dev/null; then
  echo "Format did not emit the duplicate dependency warning" >&2
  dump_output
  exit 1
fi

watch_project="$work/watch-config"
watch_out="$work/watch-config.out"
watch_err="$work/watch-config.err"
"$rescript" watch "$watch_project" >"$watch_out" 2>"$watch_err" &
watch_pid=$!
background_pids="$background_pids $watch_pid"
wait_for_file "$watch_project/src/A.js"
wait_for_text "$watch_project/lib/ocaml/.compiler.log" "#Done("
printf '{ invalid json\n' >"$watch_project/rescript.json"
wait_for_text "$watch_err" "invalid JSON"
if ! kill -0 "$watch_pid" 2>/dev/null; then
  echo "The watcher exited after a recoverable config error" >&2
  cat "$watch_out" "$watch_err" >&2
  exit 1
fi
printf '{"name":"watch-config","sources":["src"],"dependencies":["definitely-missing-dep"]}\n' \
  >"$watch_project/rescript.json"
wait_for_text "$watch_err" "definitely-missing-dep"
if ! kill -0 "$watch_pid" 2>/dev/null; then
  echo "The watcher exited after a recoverable dependency error" >&2
  cat "$watch_out" "$watch_err" >&2
  exit 1
fi
printf '{"name":"watch-config","sources":["src"],"package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}\n' \
  >"$watch_project/rescript.json"
wait_for_file "$watch_project/src/A.mjs"
wait_for_file "$watch_project/lib/bs/build.ninja"
if ! terminate_and_wait "$watch_pid" "recoverable-config watcher"; then
  cat "$watch_out" "$watch_err" >&2
  exit 1
fi
checked=$((checked + 1))

retained_graph_marker="$work/watch-retained-graph/after-build.log"
REWATCH_WATCH_FILTER_MARKER="$retained_graph_marker" \
  "$rescript" watch --after-build "node $command_work/watch-filter-marker.js" \
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
  echo "The watcher exited after an incremental dependency cycle" >&2
  exit 1
fi
printf 'let newValue = 3\n' >"$work/watch-retained-graph/src/C.res"
wait_for_line_count "$retained_graph_marker" 3
terminate_and_wait "$retained_graph_pid" "retained-graph watcher"
checked=$((checked + 1))

"$rescript" watch "$work/watch-dependency-recovery" \
  >"$work/watch-dependency-recovery.out" \
  2>"$work/watch-dependency-recovery.err" &
dependency_recovery_pid=$!
background_pids="$background_pids $dependency_recovery_pid"
wait_for_text "$work/watch-dependency-recovery.err" \
  "Could not build package tree for 'dep'"
if ! kill -0 "$dependency_recovery_pid" 2>/dev/null; then
  echo "The watcher exited after a malformed dependency config" >&2
  exit 1
fi
printf '{"name":"dep","sources":["src"]}\n' \
  >"$work/watch-dependency-recovery/packages/dep/rescript.json"
wait_for_file "$work/watch-dependency-recovery/src/A.js"
terminate_and_wait "$dependency_recovery_pid" "dependency-recovery watcher"
checked=$((checked + 1))

"$rescript" -v watch "$work/watch-dependency-install" \
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
  echo "The watcher exited while waiting for a missing dependency" >&2
  exit 1
fi
mkdir "$work/watch-dependency-install/node_modules/@scope"
wait_for_watch "$work/watch-dependency-install.out" '[/\\]@scope'
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
"$rescript" -v watch "$work/watch-dependency-fallback" \
  >"$work/watch-dependency-fallback.out" \
  2>"$work/watch-dependency-fallback.err" &
dependency_fallback_pid=$!
background_pids="$background_pids $dependency_fallback_pid"
wait_for_text "$work/watch-dependency-fallback.err" \
  "no rescript.json or bsconfig.json"
mv "$work/watch-dependency-fallback/node_modules/dep" \
  "$work/watch-dependency-fallback/node_modules/dep-disabled"
if ! wait_for_file "$work/watch-dependency-fallback/src/A.js"; then
  printf '%s\n' '--- watcher stdout ---' >&2
  cat "$work/watch-dependency-fallback.out" >&2
  printf '%s\n' '--- watcher stderr ---' >&2
  cat "$work/watch-dependency-fallback.err" >&2
  exit 1
fi
terminate_and_wait "$dependency_fallback_pid" "dependency-fallback watcher"
checked=$((checked + 1))

if $file_symlinks_supported; then
  symlink_target_marker="$work/watch-symlink-target/after-build.log"
  REWATCH_WATCH_FILTER_MARKER="$symlink_target_marker" \
    "$rescript" watch --after-build "node $command_work/watch-filter-marker.js" \
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
    echo "The watcher retained output for a dangling source symlink" >&2
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
    echo "The watcher retained output after a symlink target parent moved" >&2
    exit 1
  fi
  mv "$work/watch-symlink-external/sub.removed" \
    "$work/watch-symlink-external/sub"
  wait_for_file "$work/watch-symlink-target/src/Linked.js"
  wait_for_line_count "$symlink_target_marker" 6
  terminate_and_wait "$symlink_target_pid" "symlink-target watcher"
  checked=$((checked + 1))
fi

# The watch filter applies to file events too: an edit of a filtered-out or
# feature-disabled source does not trigger a rebuild.
filter_marker="$work/watch-filter/after-build.log"
REWATCH_WATCH_FILTER_MARKER="$filter_marker" \
  "$rescript" watch --features other --filter 'Include\.res$' \
    --after-build "node $command_work/watch-filter-marker.js" \
    "$work/watch-filter" \
    >"$work/watch-filter.out" 2>"$work/watch-filter.err" &
filter_pid=$!
background_pids="$background_pids $filter_pid"
wait_for_file "$work/watch-filter/src/Include.js"
wait_for_line_count "$filter_marker" 1
if [ -e "$work/watch-filter/src/Exclude.js" ]; then
  echo "The filter unexpectedly compiled the excluded source initially" >&2
  exit 1
fi
cp "$work/watch-filter/src/Include.js" "$work/watch-filter-initial.js"
printf 'let value = 2\n' >"$work/watch-filter/src/Include.res"
wait_for_line_count "$filter_marker" 2
if cmp -s "$work/watch-filter-initial.js" \
    "$work/watch-filter/src/Include.js" || \
  ! grep -F '2' "$work/watch-filter/src/Include.js" >/dev/null; then
  echo "The watch filter did not rebuild its included source" >&2
  cat "$work/watch-filter.out" "$work/watch-filter.err" >&2
  exit 1
fi
printf 'let value = 11\n' >"$work/watch-filter/src/Exclude.res"
if ! line_count_stays "$filter_marker" 2; then
  echo "The watch filter rebuilt for an excluded-only edit" >&2
  cat "$work/watch-filter.out" "$work/watch-filter.err" >&2
  exit 1
fi
printf 'let value = 21\n' \
  >"$work/watch-filter/inactive/Inactive.res"
if ! line_count_stays "$filter_marker" 2; then
  echo "The watcher rebuilt for a feature-disabled source edit" >&2
  cat "$work/watch-filter.out" "$work/watch-filter.err" >&2
  exit 1
fi
terminate_and_wait "$filter_pid" "filter watcher"
checked=$((checked + 1))

quiet_watch_project="$work/quiet-watch"
quiet_watch_marker="$quiet_watch_project/after-build.log"
REWATCH_WATCH_FILTER_MARKER="$quiet_watch_marker" \
  "$rescript" -q watch \
    --after-build "node $command_work/watch-filter-marker.js" \
    "$quiet_watch_project" \
    >"$work/quiet-watch.out" 2>"$work/quiet-watch.err" &
quiet_watch_pid=$!
background_pids="$background_pids $quiet_watch_pid"
wait_for_line_count "$quiet_watch_marker" 1
printf 'let value = 2\n' >"$quiet_watch_project/src/A.res"
wait_for_line_count "$quiet_watch_marker" 2
printf 'let value =\n' >"$quiet_watch_project/src/A.res"
wait_for_text "$work/quiet-watch.err" "This let-binding misses an expression"
if ! kill -0 "$quiet_watch_pid" 2>/dev/null; then
  echo "quiet-watch: watcher exited after a parse error" >&2
  exit 1
fi
printf 'let value = 3\n' >"$quiet_watch_project/src/A.res"
wait_for_line_count "$quiet_watch_marker" 3
rm "$quiet_watch_project/lib/watch.lock"
wait_for_exit "$quiet_watch_pid"
wait "$quiet_watch_pid"
if [ -s "$work/quiet-watch.out" ] || \
  grep -F "Incremental build failed" "$work/quiet-watch.err" >/dev/null; then
  echo "quiet-watch: quiet watch emitted progress or a duplicate failure summary" >&2
  cat "$work/quiet-watch.out" "$work/quiet-watch.err" >&2
  exit 1
fi
# macOS and Windows can report one write as several change events, and each
# rebuild reports the parse error again. Compare the distinct reports only.
node -e '
  const fs = require("fs");
  const blocks = fs.readFileSync(process.argv[1], "utf8").split(/^(?=Error in )/m);
  const distinct = blocks.filter((block, i) => block !== blocks[i - 1]);
  fs.writeFileSync(process.argv[2], distinct.join(""));
' "$work/quiet-watch.err" "$work/quiet-watch.err.distinct"
if ! check_snapshot command_validation/quiet-watch.txt \
  "$work/quiet-watch.err.distinct" "ROOT=$quiet_watch_project" "WORK=$work"; then
  exit 1
fi
checked=$((checked + 1))

run_case clean-missing-dependency exit2 clean "$work/missing-dependency"
run_case clean-configless-dependency exit2 clean "$work/configless-dependency"
run_case clean-malformed-dependency exit2 clean "$work/malformed-dependency"
run_rescript clean "$work/clean-duplicate"
if [ "$rescript_status" -ne 0 ] || \
  [ -e "$work/clean-duplicate/src/one/A.js" ] || \
  [ -e "$work/clean-duplicate/src/two/A.js" ] || \
  [ -e "$work/clean-duplicate/lib/bs/marker" ]; then
  echo "clean-duplicate-modules: duplicate modules were not cleaned" >&2
  dump_output
  exit 1
fi
checked=$((checked + 1))
"$rescript" watch "$work/missing-dependency" \
  >"$work/rescript.out" 2>"$work/rescript.err" &
missing_dependency_watch_pid=$!
background_pids="$background_pids $missing_dependency_watch_pid"
wait_for_text "$work/rescript.err" "Could not resolve dependency absent"
if ! kill -0 "$missing_dependency_watch_pid" 2>/dev/null; then
  echo "The watcher exited after its initial dependency error" >&2
  dump_output
  exit 1
fi
printf '{"name":"missing-dependency","sources":["src"]}\n' \
  >"$work/missing-dependency/rescript.json"
wait_for_file "$work/missing-dependency/src/A.js"
terminate_and_wait "$missing_dependency_watch_pid" \
  "missing-dependency watcher"
if $windows_posix_shell; then
  # MSYS cannot deliver the graceful console event that lets a native watcher
  # remove its locks. Its external termination intentionally leaves a stale
  # watch lock, and a build lock if the rebuild was still finishing, so remove
  # those harness artifacts before checking failure paths.
  rm -f "$work/missing-dependency/lib/watch.lock" \
    "$work/missing-dependency/lib/build.lock"
fi
printf '{"name":"missing-dependency","sources":["src"],"dependencies":["absent"]}\n' \
  >"$work/missing-dependency/rescript.json"
checked=$((checked + 1))
run_case --cwd "$work/missing-dependency" \
  format-missing-dependency exit2 format
run_case --cwd "$work/configless-dependency" \
  format-configless-dependency exit2 format
run_case --cwd "$work/malformed-dependency" \
  format-malformed-dependency exit2 format
if [ -e "$work/missing-dependency/lib/build.lock" ] || \
  [ -e "$work/missing-dependency/lib/watch.lock" ]; then
  echo "Dependency failures left a build or watch lock behind" >&2
  exit 1
fi

run_case --cwd "$project/src" format-nested reject format --check

printf 'Command validation cases: %d; expected outcomes matched\n' "$checked"
