#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")"
source ./utils.sh

bold "Embeds: full flow via Rewatch"

FIXDIR="./_tmp_embeds/rewatch_proj"
# normalize rewatch executable to absolute path so pushd doesn't break it
REWATCH_BIN=$(cd "$(dirname "$REWATCH_EXECUTABLE")" >/dev/null 2>&1 && pwd)/$(basename "$REWATCH_EXECUTABLE")
rm -rf "$FIXDIR"
mkdir -p "$FIXDIR"
cp -R ./fixtures/embeds/* "$FIXDIR"/

pushd "$FIXDIR" >/dev/null
"$REWATCH_BIN" build --snapshot-output >/dev/null 2>&1 || true
popd >/dev/null

SNAPSHOT2="../tests/snapshots/embeds-rewatch.txt"
{
  echo '=== Foo.embeds.json ==='
  cat "$FIXDIR/lib/bs/src/Foo.embeds.json" || true
  echo
  echo '=== Rewritten Source ==='
  "$RESCRIPT_BSC_EXE" -only-parse -dsource "$FIXDIR/lib/bs/src/Foo.ast" 2>/dev/null || true
  echo
  echo '=== Generated Module ==='
  # With single string embed, suffix is occurrence index 1
  cat "$FIXDIR/src/__generated__/Foo__embed_sql_one_1.res" || true
} > "$SNAPSHOT2"

normalize_paths "$SNAPSHOT2"

if git diff --exit-code ../tests/snapshots/embeds-rewatch.txt &> /dev/null;
then
  success "Rewatch embeds flow OK"
else
  error "Embeds (Rewatch) snapshot changed"
  bold ../tests/snapshots/embeds-rewatch.txt
  git --no-pager diff ../tests/snapshots/embeds-rewatch.txt ../tests/snapshots/embeds-rewatch.txt
  exit 1
fi
