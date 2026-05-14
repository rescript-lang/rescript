#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: --features=full transitively expands through the features map"

pushd ./packages/with-features > /dev/null

"$REWATCH_EXECUTABLE" clean &> /dev/null

error_output=$("$REWATCH_EXECUTABLE" build --features full 2>&1)
if [ $? -ne 0 ];
then
  error "Build with --features full failed"
  printf "%s\n" "$error_output" >&2
  popd > /dev/null
  exit 1
fi

count=$(find src src-native src-experimental -name "*.mjs" 2>/dev/null | wc -l | tr -d ' ')
if [ "$count" -eq 3 ]; then
  success "--features=full expanded to both native and experimental"
else
  error "Expected 3 .mjs files, found $count"
  find src src-native src-experimental -name "*.mjs"
  popd > /dev/null
  exit 1
fi

"$REWATCH_EXECUTABLE" clean &> /dev/null
popd > /dev/null
