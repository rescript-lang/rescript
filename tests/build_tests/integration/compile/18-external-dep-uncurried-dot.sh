#!/bin/bash
# Verifies that the legacy `(. args)` uncurried-syntax deprecation warning
# is surfaced even when it originates from an external (node_modules)
# dependency. Unrelated warnings from external deps remain suppressed.

cd $(dirname $0)
source "../utils.sh"

bold "Test: Uncurried-dot deprecation warning surfaces from external deps"

fixture=$(mktemp -d 2>/dev/null || mktemp -d -t rewatch-ext-uncur)
trap "rm -rf '$fixture'" EXIT

mkdir -p "$fixture/src"
mkdir -p "$fixture/node_modules/legacy-pkg/src"

cat > "$fixture/package.json" <<'EOF'
{
  "name": "host",
  "version": "0.0.1"
}
EOF

cat > "$fixture/rescript.json" <<'EOF'
{
  "name": "host",
  "sources": { "dir": "src" },
  "dependencies": ["legacy-pkg"],
  "package-specs": { "module": "commonjs", "in-source": true },
  "suffix": ".bs.js"
}
EOF

cat > "$fixture/src/Main.res" <<'EOF'
let _ = LegacyPkg.add(1, 2)
EOF

cat > "$fixture/node_modules/legacy-pkg/package.json" <<'EOF'
{ "name": "legacy-pkg", "version": "0.0.1" }
EOF

cat > "$fixture/node_modules/legacy-pkg/rescript.json" <<'EOF'
{
  "name": "legacy-pkg",
  "sources": { "dir": "src" },
  "package-specs": { "module": "commonjs", "in-source": true },
  "suffix": ".bs.js"
}
EOF

# Includes: the uncurried-dot deprecation (must surface) and an unused value
# (warning 26, must stay suppressed because it's in an external dep).
cat > "$fixture/node_modules/legacy-pkg/src/LegacyPkg.res" <<'EOF'
let unusedInDep = 99
let add = (. a, b) => a + b
EOF

cd "$fixture"
stderr_output=$(rewatch 2>&1 1>/dev/null)
build_status=$?

if [ $build_status -ne 0 ]; then
  error "Build failed"
  printf "%s\n" "$stderr_output" >&2
  exit 1
fi

if echo "$stderr_output" | grep -qF '`(. ...)` uncurried syntax'; then
  success "Uncurried-dot deprecation warning is shown for external dep"
else
  error "Expected '(. ...)' uncurried-syntax deprecation in stderr"
  printf "%s\n" "$stderr_output" >&2
  exit 1
fi

if echo "$stderr_output" | grep -qF 'unused value'; then
  error "Unrelated warnings from external deps should be suppressed"
  printf "%s\n" "$stderr_output" >&2
  exit 1
else
  success "Unrelated external-dep warnings are still suppressed"
fi
