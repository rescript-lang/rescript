#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")"
source ./utils.sh

bold "Embeds (compiler-only): nested expressions rewrite"

SRCDIR="./fixtures/embeds_nested/src"
BUILDDIR="./_tmp_embeds_nested/build/src"
mkdir -p "$BUILDDIR"

# 1) Emit AST + index
"$RESCRIPT_BSC_EXE" -bs-ast -o "$BUILDDIR/Foo" -embeds sql.one "$SRCDIR/Foo.res" >/dev/null 2>&1 || true

# Extract both literalHash values in order (occurrenceIndex 1..N)
LITERAL_HASH_1=$(sed -n 's/.*"literalHash"[[:space:]]*:[[:space:]]*"\([a-f0-9]\{32\}\)".*/\1/p' "$BUILDDIR/Foo.embeds.json" | sed -n '1p')
LITERAL_HASH_2=$(sed -n 's/.*"literalHash"[[:space:]]*:[[:space:]]*"\([a-f0-9]\{32\}\)".*/\1/p' "$BUILDDIR/Foo.embeds.json" | sed -n '2p')

# 2) Create resolution map for both embeds and run rewrite
cat > "$BUILDDIR/Foo.embeds.map.json" <<MAP
{
  "version": 1,
  "module": "Foo",
  "entries": [
    {
      "tag": "sql.one",
      "occurrenceIndex": 1,
      "literalHash": "$LITERAL_HASH_1",
      "targetModule": "Foo__embed_sql_one_A"
    },
    {
      "tag": "sql.one",
      "occurrenceIndex": 2,
      "literalHash": "$LITERAL_HASH_2",
      "targetModule": "Foo__embed_sql_one_B"
    }
  ]
}
MAP

"$RESCRIPT_BSC_EXE" -rewrite-embeds -ast "$BUILDDIR/Foo.ast" -map "$BUILDDIR/Foo.embeds.map.json" -o "$BUILDDIR/Foo.ast" >/dev/null 2>&1 || true

# 3) Snapshot index + rewritten source
SNAPSHOT="../tests/snapshots/embeds-nested-basic.txt"
{
  echo '=== Foo.embeds.json ==='
  cat "$BUILDDIR/Foo.embeds.json"
  echo
  echo '=== Rewritten Source ==='
  "$RESCRIPT_BSC_EXE" -only-parse -dsource "$BUILDDIR/Foo.ast" 2>/dev/null || true
} > "$SNAPSHOT"

normalize_paths "$SNAPSHOT"

if git diff --exit-code ../tests/snapshots/embeds-nested-basic.txt &> /dev/null;
then
  success "Embeds (compiler-only) nested rewrite OK"
else
  error "Embeds (compiler-only) nested snapshot changed"
  bold ../tests/snapshots/embeds-nested-basic.txt
  git --no-pager diff ../tests/snapshots/embeds-nested-basic.txt ../tests/snapshots/embeds-nested-basic.txt
  exit 1
fi
