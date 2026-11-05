#!/bin/bash
# Checks the DEBUG and TRACE events of `rescript -v build` and `-vv build`
# against snapshots/verbose_output. Only the event lines are compared, sorted,
# so parallel compilation does not reorder them.
set -eu
. "$(dirname "$0")/test_helpers.sh"

root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
rescript=${1:-$root/_build/default/rewatch-ocaml/rescript_ocaml.exe}
rescript=$(realpath "$rescript")
work=$(mktemp -d "${TMPDIR:-/tmp}/rewatch-verbose-output-XXXXXX")
cleanup() {
  rm -rf "$work"
}
trap cleanup EXIT

for level in v vv; do
  mkdir -p "$work/$level/src"
  printf '{"name":"verbose-output","sources":["src"]}\n' \
    >"$work/$level/rescript.json"
  printf 'let value = 1\n' >"$work/$level/src/A.res"
  printf 'let value = A.value\n' >"$work/$level/src/B.res"
  printf 'let value = 2\n' >"$work/$level/src/Authored.res"
  printf 'let value: int\n' >"$work/$level/src/WithInterface.resi"
  printf 'let value = 3\n' >"$work/$level/src/WithInterface.res"
done

export RESCRIPT_RUNTIME=${RESCRIPT_RUNTIME:-$root/packages/@rescript/runtime}

normalize_events() {
  input=$1
  output=$2
  sed -E \
    's# at "[^"]*rescript\.json"# at "<PROJECT>/rescript.json"#g' \
    "$input" \
    | awk '
        /^(DEBUG|TRACE):$/ {
          label = $0
          if ((getline message) > 0) print label "\t" message
          next
        }
        /^compile dirty: / { print }
      ' \
    | sort >"$output"
}

# capture NAME LEVEL: runs a build of the LEVEL project and checks its events
# against snapshots/verbose_output/NAME.txt.
capture() {
  name=$1
  level=$2
  project="$work/$level"
  (
    cd "$project"
    "$rescript" "-$level" build >"$work/$name.stdout" 2>"$work/$name.stderr"
  )
  if [ -s "$work/$name.stderr" ]; then
    echo "$name unexpectedly wrote verbose build output to stderr" >&2
    cat "$work/$name.stderr" >&2
    exit 1
  fi
  normalize_events "$work/$name.stdout" "$work/$name.events"
  check_snapshot "verbose_output/$name.txt" "$work/$name.events" \
    "PROJECT=$project"
}

capture v v
capture vv vv

if grep -E '^(TRACE:|compile dirty:)' "$work/v.events" >/dev/null; then
  echo "-v unexpectedly emitted trace-level events" >&2
  cat "$work/v.events" >&2
  exit 1
fi

printf 'let value = 2\n' >"$work/vv/src/A.res"
capture vv-incremental vv

echo "Verbose semantic output matched"
