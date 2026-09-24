#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "$0")/../.." && pwd)
mapping="$root/rewatch-ocaml/tests/rust_test_coverage.tsv"
inventory=$(mktemp)
mapped=$(mktemp)
cleanup() {
  rm -f "$inventory" "$mapped"
}
trap cleanup EXIT

while IFS= read -r file; do
  relative=${file#"$root/rewatch/src/"}
  awk -v path="$relative" '
    /#\[(tokio::)?test\]/ { pending = 1; next }
    pending && /fn[[:space:]]+[[:alnum:]_]+/ {
      line = $0
      sub(/^.*fn[[:space:]]+/, "", line)
      sub(/[^[:alnum:]_].*$/, "", line)
      print path "::" line
      pending = 0
    }
  ' "$file"
done < <(find "$root/rewatch/src" -type f -name '*.rs' \
  -exec grep -lE '#\[(tokio::)?test\]' {} + | sort) \
  | sort >"$inventory"

awk -F '\t' '
  /^[[:space:]]*#/ || /^[[:space:]]*$/ { next }
  NF != 3 { print "invalid mapping row " NR > "/dev/stderr"; invalid = 1; next }
  $2 !~ /^(covered|shared|intentional|omitted|gap|unreviewed)$/ {
    print "invalid status on mapping row " NR ": " $2 > "/dev/stderr"
    invalid = 1
  }
  { print $1 }
  END { if (invalid) exit 1 }
' "$mapping" | sort >"$mapped"

duplicates=$(uniq -d "$mapped")
if [[ -n "$duplicates" ]]; then
  printf 'Duplicate Rust test mappings:\n%s\n' "$duplicates" >&2
  exit 1
fi

missing=$(comm -23 "$inventory" "$mapped")
stale=$(comm -13 "$inventory" "$mapped")
if [[ -n "$missing" || -n "$stale" ]]; then
  [[ -z "$missing" ]] || printf 'Unmapped Rust tests:\n%s\n' "$missing" >&2
  [[ -z "$stale" ]] || printf 'Stale Rust test mappings:\n%s\n' "$stale" >&2
  exit 1
fi

total=$(wc -l <"$inventory" | tr -d ' ')
reviewed=$(awk -F '\t' '!/^#/ && NF && $2 != "unreviewed" { count++ } END { print count + 0 }' "$mapping")
gaps=$(awk -F '\t' '!/^#/ && $2 == "gap" { count++ } END { print count + 0 }' "$mapping")
unreviewed=$(awk -F '\t' '!/^#/ && $2 == "unreviewed" { count++ } END { print count + 0 }' "$mapping")
printf 'Rust unit tests: %s; reviewed: %s; gaps: %s; unreviewed: %s\n' \
  "$total" "$reviewed" "$gaps" "$unreviewed"

if [[ ${1:-} == "--require-complete" ]] && ((gaps > 0 || unreviewed > 0)); then
  exit 1
fi
