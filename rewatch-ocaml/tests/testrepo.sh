#!/bin/sh
set -eu

port="$1"
root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
: "${RESCRIPT_BSC_EXE:=$root/_build/default/compiler/bsc/rescript_compiler_main.exe}"
: "${RESCRIPT_RUNTIME:=$root/packages/@rescript/runtime}"
export RESCRIPT_BSC_EXE RESCRIPT_RUNTIME

work="$root/tmp/rewatch-ocaml/testrepo-$$"
mkdir -p "$work"
trap 'rm -rf "$work"' EXIT
cp -R "$root/rewatch/testrepo" "$work/testrepo"
rm -f "$work/testrepo/node_modules/@rescript/belt"
rm -f "$work/testrepo/node_modules/@rescript/runtime"
ln -s "$root/packages/@rescript/belt" "$work/testrepo/node_modules/@rescript/belt"
ln -s "$root/packages/@rescript/runtime" "$work/testrepo/node_modules/@rescript/runtime"

"$port" build "$work/testrepo"
test -f "$work/testrepo/src/Test.mjs"
test -f "$work/testrepo/packages/main/src/Main.mjs"
test -f "$work/testrepo/packages/new-namespace/src/NS.bs.js"

"$port" clean "$work/testrepo"
test ! -d "$work/testrepo/lib/ocaml"
test ! -d "$work/testrepo/packages/main/lib/ocaml"
test ! -d "$work/testrepo/packages/new-namespace/lib/ocaml"
test -f "$root/packages/@rescript/belt/lib/es6/src/Belt.mjs"
