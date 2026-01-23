#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: watch reports invalid experimental-features without panicking"

cp rescript.json rescript.json.bak

node -e '
const fs = require("fs");
const cfg = JSON.parse(fs.readFileSync("rescript.json", "utf8"));
cfg["experimental-features"] = ["LetUnwrap"];
fs.writeFileSync("rescript.json", JSON.stringify(cfg, null, 2));
'

out=$(rewatch watch 2>&1)
status=$?

mv rescript.json.bak rescript.json
rm -f lib/rescript.lock

if [ $status -eq 0 ]; then
  error "Expected watch to fail for invalid experimental-features list"
  echo "$out"
  exit 1
fi

echo "$out" | grep -q "Could not read rescript.json"
if [ $? -ne 0 ]; then
  error "Missing rescript.json path context in watch error"
  echo "$out"
  exit 1
fi

echo "$out" | grep -qi "experimental-features.*invalid type"
if [ $? -ne 0 ]; then
  error "Missing detailed parse error for experimental-features list"
  echo "$out"
  exit 1
fi

echo "$out" | grep -q "panicked"
if [ $? -eq 0 ]; then
  error "Watcher should not panic when config is invalid"
  echo "$out"
  exit 1
fi

success "Invalid experimental-features list handled without panic"
