#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: It should show an error for duplicate module names"

rewatch clean &> /dev/null
rewatch build &> /dev/null

cleanup_multi_entry_config() {
  git checkout -- packages/main/rescript.json &> /dev/null || true
}
trap cleanup_multi_entry_config EXIT

node -e 'const fs = require("fs"); const path = "packages/main/rescript.json"; const config = JSON.parse(fs.readFileSync(path, "utf8")); config["multi-entry"] = true; fs.writeFileSync(path, JSON.stringify(config, null, 2) + "\n");'

mkdir -p packages/main/src/lower-a packages/main/src/lower-b
echo 'let value = 1' > packages/main/src/lower-a/lowercaseDuplicate.res
echo 'let value = 2' > packages/main/src/lower-b/lowercaseDuplicate.res
if rewatch build &> /dev/null;
then
  success "Duplicate lowercase file module names are allowed"
else
  error "Duplicate lowercase file module names should be allowed"
  rm -rf packages/main/src/lower-a packages/main/src/lower-b
  exit 1
fi
rm -rf packages/main/src/lower-a packages/main/src/lower-b

mkdir -p packages/main/src/lower-private
echo 'let value = 1' > packages/main/src/lower-private/lowerPrivate.res
echo 'let value = LowerPrivate.value' > packages/main/src/lower-private/UseLowerPrivate.res
if rewatch build &> ../tests/snapshots/lowercase-file-module-private.txt;
then
  error "Lowercase file modules should not be constructible"
  rm -rf packages/main/src/lower-private
  exit 1
fi
normalize_paths ../tests/snapshots/lowercase-file-module-private.txt
if grep -q "The module or file LowerPrivate can't be found" ../tests/snapshots/lowercase-file-module-private.txt;
then
  success "Lowercase file module names are not constructible"
else
  error "Lowercase file module diagnostic changed"
  cat ../tests/snapshots/lowercase-file-module-private.txt
  rm -rf packages/main/src/lower-private
  exit 1
fi
rm -f ../tests/snapshots/lowercase-file-module-private.txt
rm -rf packages/main/src/lower-private

mkdir -p packages/main/src/dupe-a packages/main/src/dupe-b
echo 'let value = 1' > packages/main/src/dupe-a/DuplicateModule.res
echo 'let value = 2' > packages/main/src/dupe-b/DuplicateModule.res
rewatch build &> ../tests/snapshots/duplicate-module-name.txt
normalize_paths ../tests/snapshots/duplicate-module-name.txt
rm -rf packages/main/src/dupe-a packages/main/src/dupe-b

rewatch build &> /dev/null

# Check snapshot
if git diff --exit-code ../tests/snapshots/duplicate-module-name.txt &> /dev/null;
then
  success "Duplicate module name snapshot is correct"
else
  error "Duplicate module name snapshot changed"
  git diff ../tests/snapshots/duplicate-module-name.txt
  exit 1
fi
