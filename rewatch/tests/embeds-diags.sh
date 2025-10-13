#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")"
source ./utils.sh

bold "Embeds: diagnostics mapping"

FIXDIR="./_tmp_embeds/rewatch_diags_proj"
REWATCH_BIN=$(cd "$(dirname "$REWATCH_EXECUTABLE")" >/dev/null 2>&1 && pwd)/$(basename "$REWATCH_EXECUTABLE")
rm -rf "$FIXDIR"
mkdir -p "$FIXDIR"
cp -R ./fixtures/embeds_diags/* "$FIXDIR"/

pushd "$FIXDIR" >/dev/null
# Capture console output including embed diagnostics
OUTFILE="console.txt"
"$REWATCH_BIN" build --snapshot-output > "$OUTFILE" 2>&1 || true
popd >/dev/null

SNAPSHOT="../tests/snapshots/embeds-diags.txt"
{
  echo '=== Console ==='
  cat "$FIXDIR/console.txt"
} > "$SNAPSHOT"

normalize_paths "$SNAPSHOT"

if git diff --exit-code ../tests/snapshots/embeds-diags.txt &> /dev/null;
then
  success "Embeds diagnostics mapping OK"
else
  error "Embeds diagnostics snapshot changed"
  bold ../tests/snapshots/embeds-diags.txt
  git --no-pager diff ../tests/snapshots/embeds-diags.txt ../tests/snapshots/embeds-diags.txt
  exit 1
fi

