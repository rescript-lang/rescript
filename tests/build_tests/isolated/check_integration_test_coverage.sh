#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "$0")/../../.." && pwd)
tests="$root/tests/build_tests/integration"
inventory=$(mktemp)
referenced=$(mktemp)
cleanup() {
  rm -f "$inventory" "$referenced"
}
trap cleanup EXIT

for file in "$tests"/*/*.sh; do
  relative=${file#"$tests/"}
  printf './%s\n' "$relative"
done | sort >"$inventory"

sed -n 's/^\(\.\/[^ ]*\.sh\).*$/\1/p' "$tests/suite.sh" | sort \
  >"$referenced"

duplicates=$(uniq -d "$referenced")
missing=$(comm -23 "$inventory" "$referenced")
stale=$(comm -13 "$inventory" "$referenced")
if [[ -n "$duplicates" || -n "$missing" || -n "$stale" ]]; then
  [[ -z "$duplicates" ]] \
    || printf 'Integration tests referenced more than once:\n%s\n' "$duplicates" >&2
  [[ -z "$missing" ]] \
    || printf 'Integration tests omitted from suite.sh:\n%s\n' "$missing" >&2
  [[ -z "$stale" ]] \
    || printf 'Stale integration test references in suite.sh:\n%s\n' "$stale" >&2
  exit 1
fi

total=$(wc -l <"$inventory" | tr -d ' ')
printf 'Integration tests: %s; all referenced exactly once\n' "$total"
