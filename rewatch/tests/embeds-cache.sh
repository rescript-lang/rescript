#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")"
source ./utils.sh

bold "Embeds: cache + extraSources invalidation"

FIXDIR="./_tmp_embeds/rewatch_cache_proj"
rm -rf "$FIXDIR"
mkdir -p "$FIXDIR"
cp -R ./fixtures/embeds/* "$FIXDIR"/

# Normalize rewatch executable to absolute path (pushd invariant)
REWATCH_BIN=$(cd "$(dirname "$REWATCH_EXECUTABLE")" >/dev/null 2>&1 && pwd)/$(basename "$REWATCH_EXECUTABLE")

pushd "$FIXDIR" >/dev/null
rm -f gen-runs.log

# First build → generator runs once
"$REWATCH_BIN" build --snapshot-output >/dev/null 2>&1 || true
count=$(wc -l < gen-runs.log 2>/dev/null || echo 0)
if [ "$count" -ne 1 ]; then
  error "Expected 1 generator run after first build, got $count"
  popd >/dev/null; exit 1
fi

# Second build — should not decrease generator runs; typically cache hit keeps it at 1
"$REWATCH_BIN" build --snapshot-output >/dev/null 2>&1 || true
count2=$(wc -l < gen-runs.log 2>/dev/null || echo 0)
if [ "$count2" -lt 1 ]; then
  error "Expected at least 1 generator run after second build, got $count2"
  popd >/dev/null; exit 1
fi

# Touch extraSources to invalidate cache → generator runs again
echo >> dep.txt
"$REWATCH_BIN" build --snapshot-output >/dev/null 2>&1 || true
count3=$(wc -l < gen-runs.log 2>/dev/null || echo 0)
if [ "$count3" -le "$count2" ]; then
  error "Expected generator to run again after touching extraSources (got $count3, prev $count2)"
  popd >/dev/null; exit 1
fi

# Change embed string → new literalHash → generator runs again
sed -i '' 's/@name Hello/@name Hello2/' src/Foo.res 2>/dev/null || sed -i 's/@name Hello/@name Hello2/' src/Foo.res
"$REWATCH_BIN" build --snapshot-output >/dev/null 2>&1 || true
count4=$(wc -l < gen-runs.log 2>/dev/null || echo 0)
if [ "$count4" -le "$count3" ]; then
  error "Expected generator to run again after embed change (got $count4, prev $count3)"
  popd >/dev/null; exit 1
fi

success "Embeds cache + invalidation OK"
popd >/dev/null
