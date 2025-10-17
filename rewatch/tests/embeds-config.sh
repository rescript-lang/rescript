#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")"
source ./utils.sh

bold "Embeds: config embeds"

FIXDIR="./_tmp_embeds/rewatch_config_proj"
REWATCH_BIN=$(cd "$(dirname "$REWATCH_EXECUTABLE")" >/dev/null 2>&1 && pwd)/$(basename "$REWATCH_EXECUTABLE")
rm -rf "$FIXDIR"
mkdir -p "$FIXDIR"
cp -R ./fixtures/embeds_config/* "$FIXDIR"/

pushd "$FIXDIR" >/dev/null
"$REWATCH_BIN" build --snapshot-output >/dev/null 2>&1 || true

# 1) Check generated file exists with config id suffix
GEN_FILE="src/__generated__/Foo__embed_sql_one_GetUser.res"
if [ ! -f "$GEN_FILE" ]; then
  error "Generated file not found: $GEN_FILE"
  popd >/dev/null; exit 1
fi

# 2) Check header includes suffix=GetUser
if ! grep -q 'suffix=GetUser' "$GEN_FILE"; then
  error "Generated file header missing suffix=GetUser"
  popd >/dev/null; exit 1
fi

# 3) (optional) AST rewrite is exercised in other tests; here we only assert naming via generated file

success "Embeds config flow OK"
popd >/dev/null
