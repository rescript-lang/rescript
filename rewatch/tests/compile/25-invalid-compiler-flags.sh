#!/bin/bash
# A build rejects compiler-flags that the compiler would reject for every
# module, or that conflict with the flags the build system passes itself,
# once and with the location of the field. Other commands don't check them.

cd "$(dirname "$0")"
source "../utils.sh"

bold "Test: Invalid compiler flags are a configuration error"

test_dir=$(pwd)
fixture=$(mktemp -d 2>/dev/null || mktemp -d -t rewatch-compiler-flags)
trap 'cd "$test_dir"; rm -rf "$fixture"' EXIT
cd "$fixture"
mkdir src
printf 'let value = 1\n' > src/A.res

# Usage: expect_error <compiler-flags JSON> <expected error>
expect_error() {
  printf '{"name": "flags", "sources": ["src"], "compiler-flags": %s}\n' "$1" \
    > rescript.json
  if output=$(rewatch build 2>&1); then
    error "Build accepted compiler-flags $1"
    exit 1
  fi
  if ! printf '%s\n' "$output" | grep -qF "field \"compiler-flags\": $2"; then
    error "Unexpected error for compiler-flags $1"
    printf '%s\n' "$output" >&2
    exit 1
  fi
}

expect_error '["-bs-fooo"]' 'unknown flag "-bs-fooo"'
expect_error '["-bs-g", "-open"]' 'flag "-open" needs a value'
expect_error '[["-w", "-40+6"], "Belt"]' '"Belt" is not a flag'
expect_error '["-bs-ns Other"]' 'flag "-bs-ns" is set by the build system'
expect_error '["-v"]' 'flag "-v" does not compile a module'

# Formatting does not compile, so it ignores the flags.
printf 'let value =    1\n' > src/A.res
if ! output=$(rewatch format 2>&1); then
  error "Format failed because of compiler-flags"
  printf '%s\n' "$output" >&2
  exit 1
fi

printf '{"name": "flags", "sources": ["src"], "compiler-flags": ["-bs-g", "-w -40+6", "-open Stdlib"]}\n' \
  > rescript.json
if ! output=$(rewatch build 2>&1); then
  error "Build rejected valid compiler-flags"
  printf '%s\n' "$output" >&2
  exit 1
fi

success "Invalid compiler flags are a configuration error"
