#!/bin/bash
# Dead code analysis of the OCaml sources with reanalyze
# (https://github.com/rescript-lang/reanalyze). Fails when reanalyze reports
# anything.
#
# Usage: REANALYZE=/path/to/reanalyze.exe ./scripts/dead_code.sh
# (defaults to reanalyze.exe on PATH; needs a reanalyze that analyzes several
# build trees together, rescript-lang/reanalyze#224)

set -euo pipefail
cd "$(dirname "$0")/.."

reanalyze=${REANALYZE:-reanalyze.exe}

# Modules with an interface only get a .cmt in the @check target.
dune build --root . @check
# The playground (compiler/jsoo) and the playground variants of platform
# modules only build in the browser profile. Analyze both builds together so
# their uses count.
dune build --root . --profile browser --build-dir _build_playground @compiler/check

root=$(mktemp -d)
trap 'rm -rf "$root"' EXIT
ln -s "$PWD/_build/default" "$root/native"
ln -s "$PWD/_build_playground/default" "$root/playground"

# Not reported: compiler/ml/parsetree0.ml is the frozen AST of PPXs, which
# build the constructors the compiler itself never builds; it must not change.
"$reanalyze" -dce-cmt "$root" \
  -suppress compiler/ml/parsetree0.ml \
  -set-exit-code
