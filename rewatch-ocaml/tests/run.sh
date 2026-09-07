#!/bin/sh
set -eu

port="$1"
root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
: "${RESCRIPT_BSC_EXE:=$root/_build/default/compiler/bsc/rescript_compiler_main.exe}"
: "${RESCRIPT_RUNTIME:=$root/packages/@rescript/runtime}"
export RESCRIPT_BSC_EXE RESCRIPT_RUNTIME
work="$root/tmp/rewatch-ocaml/test-$$"
mkdir -p "$work"
cp -R "$root/rewatch-ocaml/tests/basic" "$work/basic"
cp -R "$root/rewatch-ocaml/tests/basic" "$work/legacy-config"
cp -R "$root/rewatch-ocaml/tests/cycle" "$work/cycle"
cp -R "$root/rewatch-ocaml/tests/failure" "$work/failure"
cp -R "$root/rewatch-ocaml/tests/features" "$work/features"
cp -R "$root/rewatch-ocaml/tests/gentype" "$work/gentype"
cp -R "$root/rewatch-ocaml/tests/dependency" "$work/dependency"
mkdir -p "$work/gentype/node_modules" "$work/dependency/node_modules"
cp -R "$root/rewatch-ocaml/tests/shared-dep" "$work/gentype/node_modules/dep"
cp -R "$root/rewatch-ocaml/tests/shared-dep" "$work/dependency/node_modules/dep"
cp -R "$root/rewatch-ocaml/tests/external-boundary" "$work/external-boundary"
cp -R "$root/rewatch-ocaml/tests/post-build" "$work/post-build"
cp -R "$root/rewatch-ocaml/tests/out-of-source" "$work/out-of-source"
cp -R "$root/rewatch-ocaml/tests/namespace" "$work/namespace"
cp -R "$root/rewatch-ocaml/tests/namespace-entry" "$work/namespace-entry"
cp -R "$root/rewatch-ocaml/tests/source-map" "$work/source-map"
cp -R "$root/rewatch-ocaml/tests/monorepo" "$work/monorepo"
basic="$work/basic"
legacy_config="$work/legacy-config"
cycle="$work/cycle"
failure="$work/failure"
features="$work/features"
gentype="$work/gentype"
dependency="$work/dependency"
external_boundary="$work/external-boundary"
post_build="$work/post-build"
out_of_source="$work/out-of-source"
namespace="$work/namespace"
namespace_entry="$work/namespace-entry"
source_map="$work/source-map"
monorepo="$work/monorepo"

"$port" compiler-args "$basic/src/A.res" | grep '"compiler_args"' >/dev/null
sed 's/"suffix": "\.mjs"/"suffix": "\.mjs", "bsc-flags": ["-w -9"]/' "$basic/rescript.json" > "$basic/rescript.next"
mv "$basic/rescript.next" "$basic/rescript.json"
sed 's/"module": "esmodule"/"module": "es6"/' "$basic/rescript.json" > "$basic/rescript.next"
mv "$basic/rescript.next" "$basic/rescript.json"
"$port" compiler-args "$basic/src/A.res" | grep '"-9"' >/dev/null
gentype_compiler_args=$("$port" compiler-args "$gentype/src/Main.res")
printf '%s\n' "$gentype_compiler_args" | grep '"-bs-gentype-generated-extension"' >/dev/null
printf '%s\n' "$gentype_compiler_args" | grep '"-bs-gentype-bsb-project-root"' >/dev/null
if printf '%s\n' "$gentype_compiler_args" | grep -E '"-bs-gentype-(dep-path|source-dir)"' >/dev/null; then
  echo "compiler-args unexpectedly included full-build GenType paths" >&2
  exit 1
fi

cleanup() {
  rm -rf "$work"
}
trap cleanup EXIT

wait_for_file() {
  file="$1"
  attempts=0
  while [ "$attempts" -lt 200 ]; do
    if [ -f "$file" ]; then
      return 0
    fi
    attempts=$((attempts + 1))
    sleep 0.1
  done
  return 1
}

wait_for_text() {
  file="$1"
  pattern="$2"
  attempts=0
  while [ "$attempts" -lt 200 ]; do
    if grep -q "$pattern" "$file" 2>/dev/null; then
      return 0
    fi
    attempts=$((attempts + 1))
    sleep 0.1
  done
  return 1
}

wait_for_file_gone() {
  file="$1"
  attempts=0
  while [ "$attempts" -lt 200 ]; do
    if [ ! -f "$file" ]; then
      return 0
    fi
    attempts=$((attempts + 1))
    sleep 0.1
  done
  return 1
}

printf 'let formatted=1\n' | "$port" format --stdin .res | grep 'let formatted = 1' >/dev/null

rm -rf "$basic/lib" "$cycle/lib" "$failure/lib"
rm -rf "$legacy_config/lib"
mv "$legacy_config/rescript.json" "$legacy_config/bsconfig.json"
rm -rf "$features/lib"
rm -rf "$gentype/lib"
rm -rf "$gentype/node_modules/dep/lib"
rm -rf "$dependency/lib" "$dependency/node_modules/dep/lib"
sed 's/"dependencies"/"bs-dependencies"/' "$dependency/rescript.json" > "$dependency/rescript.next"
mv "$dependency/rescript.next" "$dependency/rescript.json"
sed 's/}$/,"suffix":".mjs"}/' "$dependency/rescript.json" > "$dependency/rescript.next"
mv "$dependency/rescript.next" "$dependency/rescript.json"
sed 's/}$/,"suffix":".cjs"}/' "$dependency/node_modules/dep/rescript.json" > "$dependency/node_modules/dep/rescript.next"
mv "$dependency/node_modules/dep/rescript.next" "$dependency/node_modules/dep/rescript.json"
rm -rf "$post_build/lib"
rm -rf "$out_of_source/lib"
rm -rf "$namespace/lib"
rm -rf "$namespace_entry/lib"
rm -rf "$source_map/lib"
mkdir -p "$monorepo/node_modules"
ln -s ../packages/consumer "$monorepo/node_modules/consumer"
ln -s ../packages/dep "$monorepo/node_modules/dep"
rm -f "$basic/src/A.mjs" "$basic/src/B.mjs" "$basic/src/WithInterface.mjs"

"$port" build "$legacy_config"
test -f "$legacy_config/src/A.mjs"

"$port" build --filter 'A\.res$' "$basic"
test -f "$basic/src/A.mjs"
test ! -f "$basic/src/B.mjs"
rm -rf "$basic/lib"
rm -f "$basic/src/A.mjs"

"$port" build --after-build 'test -f src/A.mjs' "$basic"
test -f "$basic/src/A.mjs"
test -f "$basic/src/B.mjs"
test -f "$basic/src/WithInterface.mjs"
test -f "$basic/lib/ocaml/A.cmi"
test -f "$basic/lib/ocaml/WithInterface.cmti"
"$port" clean "$basic"
test ! -f "$basic/src/A.mjs"

watch_basic="$work/watch-basic"
cp -R "$root/rewatch-ocaml/tests/basic" "$watch_basic"
rm -rf "$watch_basic/lib"
rm -f "$watch_basic/src/A.mjs" "$watch_basic/src/B.mjs" "$watch_basic/src/WithInterface.mjs"
"$port" watch "$watch_basic" >"$watch_basic/watch.log" 2>&1 &
watch_pid=$!
if ! wait_for_file "$watch_basic/src/A.mjs"; then
  kill -TERM "$watch_pid" 2>/dev/null || true
  wait "$watch_pid" 2>/dev/null || true
  exit 1
fi
test -f "$watch_basic/lib/watch.lock"
grep '^[0-9][0-9]*$' "$watch_basic/lib/watch.lock" >/dev/null
printf '\nlet watchedValue = 1\n' >> "$watch_basic/src/B.res"
if ! wait_for_text "$watch_basic/src/B.mjs" 'watchedValue'; then
  kill -TERM "$watch_pid" 2>/dev/null || true
  wait "$watch_pid" 2>/dev/null || true
  exit 1
fi
sed 's/"\.mjs"/".js"/' "$watch_basic/rescript.json" > "$watch_basic/rescript.next"
mv "$watch_basic/rescript.next" "$watch_basic/rescript.json"
if ! wait_for_file "$watch_basic/src/A.js"; then
  cat "$watch_basic/watch.log" >&2
  kill -TERM "$watch_pid" 2>/dev/null || true
  wait "$watch_pid" 2>/dev/null || true
  exit 1
fi
test -f "$watch_basic/src/A.js"
test ! -f "$watch_basic/src/A.mjs"
printf 'let message = "new source"\n' > "$watch_basic/src/New.res"
if ! wait_for_file "$watch_basic/src/New.js"; then
  kill -TERM "$watch_pid" 2>/dev/null || true
  wait "$watch_pid" 2>/dev/null || true
  exit 1
fi
test -f "$watch_basic/src/New.js"
rm -f "$watch_basic/src/New.res"
if ! wait_for_file_gone "$watch_basic/src/New.js"; then
  kill -TERM "$watch_pid" 2>/dev/null || true
  wait "$watch_pid" 2>/dev/null || true
  exit 1
fi
test ! -f "$watch_basic/src/New.js"
kill -TERM "$watch_pid"
wait "$watch_pid"
test ! -f "$watch_basic/lib/watch.lock"

interrupt_basic="$work/interrupt-basic"
cp -R "$root/rewatch-ocaml/tests/basic" "$interrupt_basic"
cp "$root/rewatch-ocaml/tests/slow-bsc.sh" "$interrupt_basic/slow-bsc.sh"
chmod +x "$interrupt_basic/slow-bsc.sh"
child_marker="$interrupt_basic/child-started"
REWATCH_OCAML_CHILD_STARTED="$child_marker" \
REWATCH_OCAML_REAL_BSC="$RESCRIPT_BSC_EXE" \
RESCRIPT_BSC_EXE="$interrupt_basic/slow-bsc.sh" \
"$port" watch "$interrupt_basic" >"$interrupt_basic/watch.log" 2>&1 &
interrupt_pid=$!
attempts=0
while [ "$attempts" -lt 100 ] && [ ! -f "$child_marker" ]; do
  attempts=$((attempts + 1))
  sleep 0.1
done
test -f "$child_marker"
kill -TERM "$interrupt_pid"
wait "$interrupt_pid"
test ! -f "$interrupt_basic/lib/watch.lock"
test -z "$(pgrep -f "$interrupt_basic/slow-bsc.sh" || true)"
test -z "$(find "$interrupt_basic" -name '.rewatch-ocaml-*.log' -print)"

"$port" build --features native "$features"
test -f "$features/native/Native.js"

"$port" build "$gentype"
test -f "$gentype/src/Main.js"

"$port" build "$dependency"
test -f "$dependency/src/Main.js"
test -f "$dependency/node_modules/dep/src/Dep.js"
"$port" clean "$dependency"
test ! -f "$dependency/src/Main.js"
test -f "$dependency/node_modules/dep/src/Dep.js"

mkdir -p "$external_boundary/project/node_modules"
ln -s ../packages/main "$external_boundary/project/node_modules/main"
ln -s ../../external "$external_boundary/project/node_modules/external"
"$port" build "$external_boundary/project"
test -f "$external_boundary/external/src/Sentinel.js"
"$port" clean "$external_boundary/project"
test -f "$external_boundary/external/src/Sentinel.js"

"$port" build "$post_build"
test -f "$post_build/src/Main.js"

"$port" build "$out_of_source"
test -f "$out_of_source/lib/es6/src/Main.js"
rm -f "$out_of_source/src/Main.res"
"$port" build "$out_of_source"
test ! -f "$out_of_source/lib/es6/src/Main.js"

"$port" build "$namespace"
test -f "$namespace/lib/ocaml/A-Widget.cmi"
test -f "$namespace/src/B.js"

"$port" build "$namespace_entry"
test -f "$namespace_entry/src/Entry.mjs"
test -f "$namespace_entry/lib/ocaml/Entry.cmi"
test -f "$namespace_entry/lib/ocaml/Entry_alias-@EntryNamespace.cmi"

"$port" build "$source_map"
test -f "$source_map/src/Main.js.map"

sed 's/"sources":"src"/"sources":"src","dependencies":["consumer"]/' \
  "$monorepo/packages/dep/rescript.json" > "$monorepo/packages/dep/rescript.next"
mv "$monorepo/packages/dep/rescript.next" "$monorepo/packages/dep/rescript.json"
"$port" build "$monorepo"
test -f "$monorepo/src/Root.js"
test -f "$monorepo/packages/consumer/src/Consumer.js"
test -f "$monorepo/packages/dep/src/Dep.js"
"$port" clean "$monorepo"
test ! -d "$monorepo/lib/ocaml"
test ! -d "$monorepo/packages/consumer/lib/ocaml"
test ! -d "$monorepo/packages/dep/lib/ocaml"
rm -f "$features/native/Native.js"
"$port" build --features all "$features"
test -f "$features/native/Native.js"

if "$port" build "$cycle" >"$cycle/output.log" 2>&1; then
  echo "cycle build unexpectedly succeeded" >&2
  exit 1
fi
grep "circular dependency" "$cycle/output.log" >/dev/null

if "$port" build "$failure" >"$failure/output.log" 2>&1; then
  echo "invalid build unexpectedly succeeded" >&2
  exit 1
fi
grep "expected to have type" "$failure/output.log" >/dev/null

cp "$failure/Broken.fixed" "$failure/src/Broken.res"
"$port" build "$failure"
test -f "$failure/src/Broken.js"

rm -rf "$basic/lib" "$cycle/lib" "$failure/lib"
rm -rf "$legacy_config/lib"
rm -rf "$features/lib"
rm -rf "$gentype/lib"
rm -rf "$gentype/node_modules/dep/lib"
rm -rf "$dependency/lib" "$dependency/node_modules/dep/lib"
rm -rf "$post_build/lib"
rm -rf "$out_of_source/lib"
rm -rf "$namespace/lib"
rm -rf "$namespace_entry/lib"
rm -rf "$source_map/lib"
rm -f "$basic/src/A.mjs" "$basic/src/B.mjs" "$basic/src/WithInterface.mjs"
rm -f "$legacy_config/src/A.mjs" "$legacy_config/src/B.mjs" "$legacy_config/src/WithInterface.mjs"
rm -f "$cycle/output.log" "$failure/output.log"
