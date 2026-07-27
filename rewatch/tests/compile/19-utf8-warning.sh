#!/bin/bash

cd $(dirname $0)
source "../utils.sh"

bold "Test: Compiler warnings preserve UTF-8 when wrapping source lines"

fixture=$(mktemp -d 2>/dev/null || mktemp -d -t rewatch-utf8-warning)
trap "rm -rf '$fixture'" EXIT

mkdir -p "$fixture/src"

cat > "$fixture/package.json" <<'EOF'
{
  "name": "rewatch-utf8-warning",
  "version": "0.0.1"
}
EOF

cat > "$fixture/rescript.json" <<'EOF'
{
  "name": "rewatch-utf8-warning",
  "sources": { "dir": "src" },
  "warnings": { "number": "+27" }
}
EOF

cat > "$fixture/src/Main.res" <<'EOF'
let f = (~a, ~b, ~c) => ()

@doc("generic placeholder text with no real source disclosed here ....—trailing text so the line is long enough to wrap onto a second row")
let _g = 1
EOF

cd "$fixture"
compiler_output=$(rewatch build 2>&1)
build_status=$?

if [ $build_status -ne 0 ]; then
  error "Build failed"
  printf "%s\n" "$compiler_output" >&2
  exit 1
fi

if ! echo "$compiler_output" | grep -qF '—'; then
  error "Wrapped warning corrupted the em dash"
  printf "%s\n" "$compiler_output" >&2
  exit 1
fi

if echo "$compiler_output" | grep -qF '�'; then
  error "Wrapped warning contains a Unicode replacement character"
  printf "%s\n" "$compiler_output" >&2
  exit 1
fi

success "Wrapped warning preserves the em dash"
