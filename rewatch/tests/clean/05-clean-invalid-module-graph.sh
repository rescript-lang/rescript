#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: Clean succeeds when the module graph is invalid"

rewatch clean > /dev/null 2>&1
cp rescript.json rescript.json.clean-invalid.bak

restore_fixture() {
  rm -rf src/dupe-a src/dupe-b
  if [ -f rescript.json.clean-invalid.bak ]; then
    mv rescript.json.clean-invalid.bak rescript.json
  fi
  rewatch build > /dev/null 2>&1 || true
}
trap restore_fixture EXIT

node -e '
  const fs = require("fs");
  const path = process.argv[1];
  const config = JSON.parse(fs.readFileSync(path, "utf8"));
  config["package-specs"] = [
    {module: "esmodule", "in-source": true},
    {module: "esmodule", "in-source": false, suffix: ".out.mjs"},
  ];
  fs.writeFileSync(path, `${JSON.stringify(config, null, 2)}\n`);
' rescript.json

rewatch build > /dev/null 2>&1

if [ ! -f src/Test.mjs ] || [ ! -f lib/es6/src/Test.out.mjs ]; then
  error "Test setup did not produce both in-source and out-of-source outputs"
  exit 1
fi

mkdir -p src/dupe-a src/dupe-b
echo 'let value = 1' > src/dupe-a/DuplicateModule.res
echo 'let value = 2' > src/dupe-b/DuplicateModule.res

error_output=$(rewatch clean 2>&1)
clean_status=$?

if [ $clean_status -ne 0 ]; then
  error "Clean failed for an invalid module graph"
  printf "%s\n" "$error_output" >&2
  exit 1
fi

if [ -f src/Test.mjs ] || [ -f lib/es6/src/Test.out.mjs ]; then
  error "Clean left a generated output behind"
  exit 1
fi

restore_fixture
trap - EXIT
success "Invalid module graph cleaned"
