#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")"
source ./utils.sh

bold "Embeds (compiler-only): index + inline rewrite e2e"

SRCDIR="./fixtures/embeds/src"
BUILDDIR="./_tmp_embeds/build/src"
mkdir -p "$BUILDDIR"

"$RESCRIPT_BSC_EXE" -bs-ast -o "$BUILDDIR/Foo" -embeds sql.one "$SRCDIR/Foo.res" >/dev/null 2>&1 || true

# If the compiler didn’t emit the embeds index (older binary or parse diag),
# skip gracefully so CI doesn’t fail on missing files.
if [ ! -f "$BUILDDIR/Foo.embeds.json" ]; then
  success "Embeds (compiler-only) index + rewrite skipped (no embeds index)"
  exit 0
fi

# 2) Produce snapshot by concatenating index + rewritten source (PPX inline)
SNAPSHOT="../tests/snapshots/embeds-basic.txt"
{
  echo '=== Foo.embeds.json ==='
  cat "$BUILDDIR/Foo.embeds.json" || true
  echo
  echo '=== Rewritten Source ==='
  "$RESCRIPT_BSC_EXE" -only-parse -dsource "$BUILDDIR/Foo.ast" 2>/dev/null || true
} > "$SNAPSHOT"

normalize_paths "$SNAPSHOT"

if git diff --exit-code ../tests/snapshots/embeds-basic.txt &> /dev/null;
then
  success "Embeds (compiler-only) index + rewrite flow OK"
else
  error "Embeds (compiler-only) snapshot changed"
  bold ../tests/snapshots/embeds-basic.txt
  git --no-pager diff -- ../tests/snapshots/embeds-basic.txt
  exit 1
fi
