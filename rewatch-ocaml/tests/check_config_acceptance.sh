#!/bin/bash
# Runs `rescript compiler-args` on every rescript.json in
# config_acceptance_cases.tsv. Each case must be accepted or rejected as the
# table says. The compiler and parser arguments of the accepted cases are
# compared with snapshots/config_acceptance/arguments.txt.
set -eu
. "$(dirname "$0")/test_helpers.sh"

root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
rescript=${1:-$root/_build/default/rewatch-ocaml/rescript_ocaml.exe}
cases=$root/rewatch-ocaml/tests/config_acceptance_cases.tsv

rescript=$(realpath "$rescript")
mkdir -p "$root/tmp"
work=$(mktemp -d "$root/tmp/rewatch-config-acceptance-XXXXXX")
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/project/src" "$work/project/node_modules/dep/lib/ocaml" \
  "$work/project/node_modules/ppx"
project=$work/project
printf 'let value = 1\n' >"$project/src/A.res"

default_runtime=$root/packages/@rescript/runtime
if $windows_posix_shell; then
  default_runtime=$(cygpath -w "$default_runtime")
fi
export RESCRIPT_RUNTIME=${RESCRIPT_RUNTIME:-$default_runtime}

checked=0
diagnostic_checks=0
: >"$work/arguments.txt"
while IFS=$'\t' read -r area name expected json; do
  if [[ -z "$area" || "$area" == \#* ]]; then
    continue
  fi
  printf '%s\n' "$json" >"$project/rescript.json"
  set +e
  "$rescript" compiler-args "$project/src/A.res" >"$work/out" 2>"$work/err"
  status=$?
  set -e

  if [[ "$status" -eq 0 ]]; then
    actual=accept
  else
    actual=reject
  fi
  if [[ "$actual" != "$expected" ]]; then
    printf 'Config case %s/%s: expected %s, got status %s\n' \
      "$area" "$name" "$expected" "$status" >&2
    cat "$work/out" "$work/err" >&2
    exit 1
  fi
  diagnostic_fragment=
  case "$area" in
    jsx) diagnostic_fragment=jsx ;;
    source-map) diagnostic_fragment=sourceMap ;;
  esac
  if [[ -n "$diagnostic_fragment" && "$expected" != accept ]]; then
    if ! grep -Fi "$diagnostic_fragment" "$work/err" >/dev/null; then
      printf 'Config case %s/%s lost the %s diagnostic context\n' \
        "$area" "$name" "$diagnostic_fragment" >&2
      cat "$work/out" "$work/err" >&2
      exit 1
    fi
    diagnostic_checks=$((diagnostic_checks + 1))
  fi
  if [[ "$expected" == accept ]]; then
    # One line per argument list, so native Windows paths are not JSON-escaped
    # and check_snapshot can replace them.
    node -e '
      const fs = require("fs");
      const [file, label] = process.argv.slice(1);
      const args = JSON.parse(fs.readFileSync(file, "utf8"));
      fs.appendFileSync(
        process.argv[3],
        `${label}\n  compiler: ${args.compiler_args.join(" ")}\n` +
          `  parser: ${args.parser_args.join(" ")}\n`,
      );
    ' "$work/out" "$area/$name" "$work/arguments.txt"
  fi
  checked=$((checked + 1))
done <"$cases"

check_snapshot config_acceptance/arguments.txt "$work/arguments.txt" \
  "PROJECT=$project" "RUNTIME=$RESCRIPT_RUNTIME"

printf 'Configuration cases: %d (%d JSX/source-map diagnostic checks); expectations and arguments matched\n' \
  "$checked" "$diagnostic_checks"
