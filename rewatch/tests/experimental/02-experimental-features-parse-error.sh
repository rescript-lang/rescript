#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: build surfaces experimental-features list parse error"

cp rescript.json rescript.json.bak

node -e '
const fs = require("fs");
const j = JSON.parse(fs.readFileSync("rescript.json", "utf8"));
j["experimental-features"] = ["LetUnwrap"];
fs.writeFileSync("rescript.json", JSON.stringify(j, null, 2));
'

out=$(rewatch build 2>&1)
status=$?

mv rescript.json.bak rescript.json

if [ $status -eq 0 ]; then
  error "Expected build to fail for experimental-features list input"
  echo "$out"
  exit 1
fi

echo "$out" | grep -q "Could not read rescript.json"
if [ $? -ne 0 ]; then
  error "Missing rescript.json path context in build error"
  echo "$out"
  exit 1
fi

echo "$out" | grep -qi "experimental-features.*invalid type"
if [ $? -ne 0 ]; then
  error "Missing detailed parse error in build output"
  echo "$out"
  exit 1
fi

success "Experimental-features list produces detailed build error"
