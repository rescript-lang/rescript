#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: --features=native restricts compilation to the untagged and native source dirs"

pushd ./packages/with-features > /dev/null

"$REWATCH_EXECUTABLE" clean &> /dev/null

error_output=$("$REWATCH_EXECUTABLE" build --features native 2>&1)
if [ $? -ne 0 ];
then
  error "Build with --features native failed"
  printf "%s\n" "$error_output" >&2
  popd > /dev/null
  exit 1
fi

if [ -f "src/Common.mjs" ]; then
  success "Untagged source src/Common.res was compiled"
else
  error "Expected src/Common.mjs to exist"
  popd > /dev/null
  exit 1
fi

if [ -f "src-native/Native.mjs" ]; then
  success "Feature-gated source src-native/Native.res was compiled"
else
  error "Expected src-native/Native.mjs to exist"
  popd > /dev/null
  exit 1
fi

if [ -f "src-experimental/Experimental.mjs" ]; then
  error "Did not expect src-experimental/Experimental.mjs to be compiled"
  popd > /dev/null
  exit 1
else
  success "Feature-gated source src-experimental/Experimental.res was skipped"
fi

"$REWATCH_EXECUTABLE" clean &> /dev/null
popd > /dev/null
