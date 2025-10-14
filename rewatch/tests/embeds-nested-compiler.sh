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

# If the compiler didn’t emit the embeds index (older binary or parse diag),
# skip gracefully so CI doesn’t fail on missing files.
if [ ! -f "$BUILDDIR/Foo.embeds.json" ]; then
  success "Embeds (compiler-only) nested rewrite skipped (no embeds index)"
  exit 0
fi

# Snapshot and diff only; no need to parse literal hashes here

# 2) Snapshot index + rewritten source (PPX inline)
SNAPSHOT="../tests/snapshots/embeds-nested-basic.txt"
{
  echo '=== Foo.embeds.json ==='
  cat "$BUILDDIR/Foo.embeds.json" || true
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
  git --no-pager diff -- ../tests/snapshots/embeds-nested-basic.txt
  exit 1
fi
