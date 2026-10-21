#!/bin/bash

cd $(dirname $0)
source "../utils.sh"

bold "Test: Modules of source dir \".\" are not rebuilt when nothing changed"

fixture=$(mktemp -d 2>/dev/null || mktemp -d -t rewatch-root-source-dir)
trap "rm -rf '$fixture'" EXIT

mkdir -p "$fixture/ui"

# "." spells its modules "./Root.res" unless the build system drops the
# segment, while bsc records "Root.res" in the AST and outputs go to "Root.mjs"
# and "ui/Component.mjs". A mismatch makes every build clean those artifacts.
cat > "$fixture/rescript.json" <<'JSON'
{
  "name": "rewatch-root-source-dir",
  "sources": [{ "dir": "." }, { "dir": "ui" }],
  "package-specs": { "module": "esmodule", "in-source": true },
  "suffix": ".mjs"
}
JSON

cat > "$fixture/Root.res" <<'RES'
let value = 1
RES

cat > "$fixture/ui/Component.res" <<'RES'
let value = Root.value + 1
RES

cd "$fixture"
if ! compiler_output=$("$REWATCH_EXECUTABLE" build 2>&1); then
  error "Initial fixture build failed"
  printf "%s\n" "$compiler_output" >&2
  exit 1
fi

# Several unchanged builds, since a removed output only shows on the next one.
for attempt in 1 2 3; do
  compiler_output=$("$REWATCH_EXECUTABLE" build 2>&1)
  if ! printf '%s\n' "$compiler_output" | grep -q "Compiled 0 modules"; then
    error "Unchanged build $attempt recompiled modules"
    printf "%s\n" "$compiler_output" >&2
    exit 1
  fi
done

if [ ! -f Root.mjs ] || [ ! -f ui/Component.mjs ]; then
  error "Unchanged builds removed generated outputs"
  exit 1
fi

success "Modules of source dir \".\" are not rebuilt when nothing changed"
