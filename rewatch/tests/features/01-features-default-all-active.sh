#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: with no --features flag, all feature-tagged source dirs are compiled"

pushd ./packages/with-features > /dev/null

"$REWATCH_EXECUTABLE" clean &> /dev/null

error_output=$("$REWATCH_EXECUTABLE" build 2>&1)
if [ $? -ne 0 ];
then
  error "Default build should succeed"
  printf "%s\n" "$error_output" >&2
  popd > /dev/null
  exit 1
fi

for dir in src src-native src-experimental; do
  if [ -f "$dir/${dir#src-}.mjs" ] || [ -f "$dir/Common.mjs" ] || [ -f "$dir/Native.mjs" ] || [ -f "$dir/Experimental.mjs" ]; then
    found=1
  else
    found=0
  fi
done

count=$(find src src-native src-experimental -name "*.mjs" 2>/dev/null | wc -l | tr -d ' ')
if [ "$count" -eq 3 ]; then
  success "All 3 source dirs compiled without --features"
else
  error "Expected 3 .mjs files (one per source dir), found $count"
  find src src-native src-experimental -name "*.mjs"
  popd > /dev/null
  exit 1
fi

"$REWATCH_EXECUTABLE" clean &> /dev/null
popd > /dev/null
