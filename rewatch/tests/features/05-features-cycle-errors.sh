#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: a cycle in the features map produces a clear error"

pushd ./packages/with-features > /dev/null

"$REWATCH_EXECUTABLE" clean &> /dev/null

cp rescript.json rescript.json.bak
cat > rescript.json <<'JSON'
{
    "name": "@testrepo/with-features",
    "sources": [
        { "dir": "src" },
        { "dir": "src-native", "feature": "native" },
        { "dir": "src-experimental", "feature": "experimental" }
    ],
    "package-specs": {
        "module": "esmodule",
        "in-source": true
    },
    "suffix": ".mjs",
    "features": {
        "a": ["b"],
        "b": ["a"]
    }
}
JSON

error_output=$("$REWATCH_EXECUTABLE" build --features a 2>&1)
status=$?

mv rescript.json.bak rescript.json

if [ "$status" -eq 0 ]; then
  error "Expected build to fail on cycle, but it succeeded"
  popd > /dev/null
  exit 1
fi

if echo "$error_output" | grep -q "Cycle detected"; then
  success "Cycle in features map produced a clear error"
else
  error "Expected error message to mention cycle, got: $error_output"
  popd > /dev/null
  exit 1
fi

"$REWATCH_EXECUTABLE" clean &> /dev/null
popd > /dev/null
