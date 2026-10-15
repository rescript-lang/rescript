#!/bin/bash
# Verifies that a source dependency is rebuilt when the consuming project's
# package output settings change.

cd $(dirname $0)
source "../utils.sh"

bold "Test: Rebuild dependencies when package specs change"

fixture=$(mktemp -d 2>/dev/null || mktemp -d -t rewatch-package-specs)
trap "rm -rf '$fixture'" EXIT

mkdir -p "$fixture/src"
mkdir -p "$fixture/node_modules/shared-dep/src"

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
  "dependencies": ["shared-dep"],
  "package-specs": { "module": "commonjs", "in-source": false, "suffix": ".cjs" }
}
EOF

cat > "$fixture/src/Main.res" <<'EOF'
let value = SharedDep.value
EOF

cat > "$fixture/node_modules/shared-dep/package.json" <<'EOF'
{
  "name": "shared-dep",
  "version": "0.0.1"
}
EOF

cat > "$fixture/node_modules/shared-dep/rescript.json" <<'EOF'
{
  "name": "shared-dep",
  "sources": { "dir": "src" }
}
EOF

cat > "$fixture/node_modules/shared-dep/src/SharedDep.res" <<'EOF'
let value = 42
EOF

cd "$fixture"
rewatch build

if [ ! -f "node_modules/shared-dep/lib/js/src/SharedDep.cjs" ]; then
  error "Expected CommonJS dependency output"
  exit 1
fi

cat > "$fixture/rescript.json" <<'EOF'
{
  "name": "host",
  "sources": { "dir": "src" },
  "dependencies": ["shared-dep"],
  "package-specs": { "module": "esmodule", "in-source": false, "suffix": ".mjs" }
}
EOF

rewatch build

if [ ! -f "node_modules/shared-dep/lib/es6/src/SharedDep.mjs" ]; then
  error "Expected ES module dependency output after changing package specs"
  exit 1
fi

if [ -f "node_modules/shared-dep/lib/js/src/SharedDep.cjs" ]; then
  error "Expected previous CommonJS dependency output to be removed"
  exit 1
fi

success "Source dependency was rebuilt for the new package specs"
