#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: --features with empty value is rejected"

pushd ./packages/with-features > /dev/null

error_output=$("$REWATCH_EXECUTABLE" build --features "" 2>&1)
status=$?

if [ "$status" -eq 0 ]; then
  error "Expected empty --features to fail, but build succeeded"
  popd > /dev/null
  exit 1
fi

if echo "$error_output" | grep -q "must not be empty"; then
  success "Empty --features value is rejected with a helpful message"
else
  error "Expected error mentioning 'must not be empty', got: $error_output"
  popd > /dev/null
  exit 1
fi

popd > /dev/null
