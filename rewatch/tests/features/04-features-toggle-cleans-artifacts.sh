#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: toggling off a feature removes previously-compiled artifacts on next build"

pushd ./packages/with-features > /dev/null

"$REWATCH_EXECUTABLE" clean &> /dev/null

# First build with all features: experimental should be compiled
error_output=$("$REWATCH_EXECUTABLE" build 2>&1)
if [ $? -ne 0 ]; then
  error "Initial full build failed"
  printf "%s\n" "$error_output" >&2
  popd > /dev/null
  exit 1
fi

if [ ! -f "src-experimental/Experimental.mjs" ]; then
  error "Expected src-experimental/Experimental.mjs from initial build"
  popd > /dev/null
  exit 1
fi

# Rebuild with only 'native' active
error_output=$("$REWATCH_EXECUTABLE" build --features native 2>&1)
if [ $? -ne 0 ]; then
  error "Restricted rebuild failed"
  printf "%s\n" "$error_output" >&2
  popd > /dev/null
  exit 1
fi

if [ -f "src-experimental/Experimental.mjs" ]; then
  error "Expected src-experimental/Experimental.mjs to be cleaned up when feature disabled"
  popd > /dev/null
  exit 1
else
  success "Old experimental artifact was removed after feature was disabled"
fi

if [ ! -f "src-native/Native.mjs" ]; then
  error "Expected src-native/Native.mjs to still exist"
  popd > /dev/null
  exit 1
fi

"$REWATCH_EXECUTABLE" clean &> /dev/null
popd > /dev/null
