#!/bin/bash
cd $(dirname $0)
source "./utils.sh"
cd ../testrepo

bold "Test: invalid experimental-features keys produce helpful error"

cp rescript.json rescript.json.bak

node -e '
const fs=require("fs");
const j=JSON.parse(fs.readFileSync("rescript.json","utf8"));
j["experimental-features"]={FooBar:true};
fs.writeFileSync("rescript.json", JSON.stringify(j,null,2));
'

out=$(rewatch compiler-args packages/file-casing/src/Consume.res 2>&1)
status=$?

mv rescript.json.bak rescript.json

if [ $status -eq 0 ]; then
  error "Expected compiler-args to fail for unknown experimental feature"
  echo "$out"
  exit 1
fi

echo "$out" | grep -q "Unknown experimental feature 'FooBar'. Available features: LetUnwrap"
if [ $? -ne 0 ]; then
  error "Missing helpful message for unknown experimental feature"
  echo "$out"
  exit 1
fi

success "invalid experimental-features produces helpful error"

