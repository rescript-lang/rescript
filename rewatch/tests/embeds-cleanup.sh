#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")"
source ./utils.sh

bold "Embeds: stale cleanup"

FIXDIR="./_tmp_embeds/rewatch_cleanup_proj"
rm -rf "$FIXDIR"
mkdir -p "$FIXDIR"
cp -R ./fixtures/embeds/* "$FIXDIR"/

# Normalize rewatch executable to absolute path (pushd invariant)
REWATCH_BIN=$(cd "$(dirname "$REWATCH_EXECUTABLE")" >/dev/null 2>&1 && pwd)/$(basename "$REWATCH_EXECUTABLE")

pushd "$FIXDIR" >/dev/null

# 1) Initial build → generates Hello module
"$REWATCH_BIN" build --snapshot-output >/dev/null 2>&1 || true
if [ ! -f src/__generated__/Foo__embed_sql_one_Hello.res ]; then
  error "Expected generated Hello file missing"
  popd >/dev/null; exit 1
fi

# 2) Remove embed entirely; all Module__embed_ files for Foo should be cleaned
# Portable replace of the line to 'let a = 1'
awk '{ if ($1=="let" && $2=="a" && $3=="=") print "let a = 1"; else print $0 }' ./src/Foo.res > ./src/Foo.res.tmp && mv ./src/Foo.res.tmp ./src/Foo.res
"$REWATCH_BIN" build --snapshot-output >/dev/null 2>&1 || true
if ls src/__generated__/Foo__embed_* 1>/dev/null 2>&1; then
  echo "Current generated files:"
  ls -la src/__generated__ || true
  error "Stale generated files not removed after embed deletion"
  popd >/dev/null; exit 1
fi

success "Embeds stale cleanup OK"
popd >/dev/null
