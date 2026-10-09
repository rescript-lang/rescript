#!/bin/bash

cd $(dirname $0)
source "../utils.sh"

bold "Test: Independent modules compile after an unrelated error"

fixture=$(mktemp -d 2>/dev/null || mktemp -d -t rewatch-schedule-after-error)
trap "rm -rf '$fixture'" EXIT

mkdir -p "$fixture/src"

cat > "$fixture/rescript.json" <<'EOF'
{
  "name": "rewatch-schedule-after-error",
  "sources": { "dir": "src" },
  "warnings": { "number": "+26+27+32", "error": false },
  "package-specs": { "module": "esmodule", "in-source": true },
  "suffix": ".mjs"
}
EOF

cat > "$fixture/src/Failing.res" <<'EOF'
let value = 1
EOF

cat > "$fixture/src/Blocked.res" <<'EOF'
let mirrored = Failing.value
EOF

cat > "$fixture/src/IndependentWarning.res" <<'EOF'
let unusedValue = 42

let hello = () => Console.log("hello")
EOF

cd "$fixture"
if ! RAYON_NUM_THREADS=1 RUST_BACKTRACE=1 "$REWATCH_EXECUTABLE" build > /dev/null 2>&1; then
  error "Initial fixture build failed"
  exit 1
fi

# Make the dependent's generated output an observable signal: the failing build
# must leave it absent, and the recovery build must recreate it.
rm -f src/Blocked.mjs

cat > src/Failing.res <<'EOF'
let value: int = "not an int"
EOF

cat > src/IndependentWarning.res <<'EOF'
let unusedValue = 42

let hello = () => {
  let secondUnusedValue = 43
  Console.log("hello")
}
EOF

compiler_output=$(RAYON_NUM_THREADS=1 RUST_BACKTRACE=1 "$REWATCH_EXECUTABLE" build 2>&1)
build_status=$?

if [ $build_status -eq 0 ]; then
  error "Build unexpectedly succeeded despite the type error"
  printf "%s\n" "$compiler_output" >&2
  exit 1
fi

if ! printf '%s\n' "$compiler_output" | grep -q "unused variable secondUnusedValue"; then
  error "Independent warning was not emitted after the unrelated failure"
  printf "%s\n" "$compiler_output" >&2
  exit 1
fi

if [ -f src/Blocked.mjs ]; then
  error "Blocked dependent was compiled despite its failed dependency"
  exit 1
fi

printf 'let value = "recovered"\n' > src/Failing.res
if ! compiler_output=$(RAYON_NUM_THREADS=1 RUST_BACKTRACE=1 "$REWATCH_EXECUTABLE" build 2>&1); then
  error "Build did not recover after fixing the failed dependency"
  printf "%s\n" "$compiler_output" >&2
  exit 1
fi

if [ ! -f src/Blocked.mjs ]; then
  error "Previously blocked dependent was not compiled after recovery"
  exit 1
fi

success "Independent diagnostics and blocked dependents are scheduled correctly"
