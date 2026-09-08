#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "$0")/../.." && pwd)
tests="$root/rewatch/tests"
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
    || printf 'Canonical tests referenced more than once:\n%s\n' "$duplicates" >&2
  [[ -z "$missing" ]] \
    || printf 'Canonical tests omitted from suite.sh:\n%s\n' "$missing" >&2
  [[ -z "$stale" ]] \
    || printf 'Stale canonical test references in suite.sh:\n%s\n' "$stale" >&2
  exit 1
fi

total=$(wc -l <"$inventory" | tr -d ' ')
printf 'Canonical integration tests: %s; all referenced exactly once\n' "$total"
