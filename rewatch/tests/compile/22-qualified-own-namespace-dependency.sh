#!/bin/bash

cd $(dirname $0)
source "../utils.sh"

bold "Test: A module reached only through its own namespace is a dependency"

fixture=$(mktemp -d 2>/dev/null || mktemp -d -t rewatch-qualified-own-namespace)
trap "rm -rf '$fixture'" EXIT

mkdir -p "$fixture/src"

cat > "$fixture/rescript.json" <<'JSON'
{
  "name": "rewatch-qualified-own-namespace",
  "namespace": "Ns",
  "sources": { "dir": "src" },
  "package-specs": { "module": "esmodule", "in-source": true },
  "suffix": ".mjs"
}
JSON

# User names Target only as Ns.Target, so its AST header must say so: the
# bare namespace does not tell the build system which module to wait for.
cat > "$fixture/src/Target.res" <<'RES'
let value = 1
RES

cat > "$fixture/src/User.res" <<'RES'
let value = Ns.Target.value + 1
RES

cd "$fixture"
if ! compiler_output=$("$REWATCH_EXECUTABLE" build 2>&1); then
  error "Clean build compiled User before Target"
  printf "%s\n" "$compiler_output" >&2
  exit 1
fi

# Changing Target must recompile User and report its new type error.
cat > src/Target.res <<'RES'
let value = "not an int"
RES

if compiler_output=$("$REWATCH_EXECUTABLE" build 2>&1); then
  error "Incremental build did not recompile User after Target changed"
  printf "%s\n" "$compiler_output" >&2
  exit 1
fi

if ! printf '%s\n' "$compiler_output" | grep -q "User.res"; then
  error "Incremental build did not report the error in User"
  printf "%s\n" "$compiler_output" >&2
  exit 1
fi

success "Modules reached through their own namespace are dependencies"
