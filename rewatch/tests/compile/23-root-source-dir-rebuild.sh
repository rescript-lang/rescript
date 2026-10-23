#!/bin/bash

cd $(dirname $0)
source "../utils.sh"

bold "Test: Modules of source dir \".\" are not rebuilt when nothing changed"

fixtures=$(mktemp -d 2>/dev/null || mktemp -d -t rewatch-root-source-dir)
trap "rm -rf '$fixtures'" EXIT

# "." spells its modules "./Root.res" unless the build system drops the
# segment, while bsc records "Root.res" in the AST and the output inventory
# finds "Root.mjs" or "lib/es6/Root.mjs". A mismatch makes every build clean
# those artifacts.
check_layout() {
  local name=$1 in_source=$2 root_output=$3 ui_output=$4
  local fixture="$fixtures/$name"
  mkdir -p "$fixture/ui"

  cat > "$fixture/rescript.json" <<JSON
{
  "name": "rewatch-root-source-dir-$name",
  "sources": [{ "dir": "." }, { "dir": "ui" }],
  "package-specs": { "module": "esmodule", "in-source": $in_source },
  "suffix": ".mjs"
}
JSON

  printf 'let value = 1\n' > "$fixture/Root.res"
  printf 'let value = Root.value + 1\n' > "$fixture/ui/Component.res"

  if ! compiler_output=$(cd "$fixture" && "$REWATCH_EXECUTABLE" build 2>&1); then
    error "Initial $name fixture build failed"
    printf "%s\n" "$compiler_output" >&2
    return 1
  fi

  # Several unchanged builds, since a removed output only shows on the next one.
  for attempt in 1 2 3; do
    compiler_output=$(cd "$fixture" && "$REWATCH_EXECUTABLE" build 2>&1)
    if ! printf '%s\n' "$compiler_output" | grep -q "Compiled 0 modules"; then
      error "Unchanged $name build $attempt recompiled modules"
      printf "%s\n" "$compiler_output" >&2
      return 1
    fi
  done

  if [ ! -f "$fixture/$root_output" ] || [ ! -f "$fixture/$ui_output" ]; then
    error "Unchanged $name builds removed generated outputs"
    return 1
  fi
}

check_layout in-source true Root.mjs ui/Component.mjs || exit 1
check_layout out-of-source false lib/es6/Root.mjs lib/es6/ui/Component.mjs || exit 1

success "Modules of source dir \".\" are not rebuilt when nothing changed"
