#!/bin/bash
cd $(dirname $0)
source "./utils.sh"
cd ../testrepo

bold "Test: compile-file command should output JS to stdout"

# Build first to ensure artifacts exist
error_output=$(rewatch build 2>&1)
if [ $? -ne 0 ]; then
  error "Error building repo"
  printf "%s\n" "$error_output" >&2
  exit 1
fi

# Test 1: Basic compilation - stdout should contain valid JavaScript
bold "Test: Compile outputs valid JavaScript"
stdout=$(rewatch compile-file packages/main/src/Main.res 2>/dev/null)
if [ $? -ne 0 ]; then
  error "Error compiling packages/main/src/Main.res"
  exit 1
fi

# Check stdout contains JS (look for common JS patterns)
if echo "$stdout" | grep -q "export\|function\|import" ; then
  success "compile outputs JavaScript to stdout"
else
  error "compile stdout doesn't look like JavaScript"
  echo "$stdout"
  exit 1
fi

# Test 2: Compilation from subdirectory should work
bold "Test: Compile works from subdirectory"
pushd packages/main > /dev/null
stdout=$("$REWATCH_EXECUTABLE" compile-file src/Main.res 2>/dev/null)
if [ $? -eq 0 ]; then
  success "compile works from subdirectory"
else
  error "compile failed from subdirectory"
  popd > /dev/null
  exit 1
fi
popd > /dev/null

# Test 3: Errors should go to stderr, not stdout
bold "Test: Errors go to stderr, not stdout"
stdout=$(rewatch compile-file packages/main/src/NonExistent.res 2>/dev/null)
stderr=$(rewatch compile-file packages/main/src/NonExistent.res 2>&1 >/dev/null)
if [ -z "$stdout" ] && [ -n "$stderr" ]; then
  success "Errors correctly sent to stderr"
else
  error "Errors not correctly handled"
  echo "stdout: $stdout"
  echo "stderr: $stderr"
  exit 1
fi

success "All compile-one tests passed"

