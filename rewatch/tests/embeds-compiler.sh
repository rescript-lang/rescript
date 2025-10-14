#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")"
source ./utils.sh

bold "Embeds (compiler-only): index + inline rewrite e2e"

SRCDIR="./fixtures/embeds/src"
BUILDDIR="./_tmp_embeds/build/src"
mkdir -p "$BUILDDIR"

# 1) Emit AST + index
"$RESCRIPT_BSC_EXE" -bs-ast -o "$BUILDDIR/Foo" -embeds sql.one "$SRCDIR/Foo.res" >/dev/null 2>&1 || true

# Extract the literalHash from the index (regex; jq not required)
LITERAL_HASH=$(sed -n 's/.*"literalHash"[[:space:]]*:[[:space:]]*"\([a-f0-9]\{32\}\)".*/\1/p' "$BUILDDIR/Foo.embeds.json" | head -n1)

# 2) Produce snapshot by concatenating index + rewritten source (PPX inline)
SNAPSHOT="../tests/snapshots/embeds-basic.txt"
{
  echo '=== Foo.embeds.json ==='
  cat "$BUILDDIR/Foo.embeds.json"
  echo
  echo '=== Rewritten Source ==='
  "$RESCRIPT_BSC_EXE" -only-parse -dsource "$BUILDDIR/Foo.ast" 2>/dev/null || true
} > "$SNAPSHOT"

normalize_paths "$SNAPSHOT"

changed_snapshots=$(git ls-files --modified ../tests/snapshots/embeds-basic.txt)
if git diff --exit-code ../tests/snapshots/embeds-basic.txt &> /dev/null;
then
  success "Embeds (compiler-only) index + rewrite flow OK"
else
  error "Embeds (compiler-only) snapshot changed"
  bold ../tests/snapshots/embeds-basic.txt
  git --no-pager diff ../tests/snapshots/embeds-basic.txt ../tests/snapshots/embeds-basic.txt
  exit 1
fi
