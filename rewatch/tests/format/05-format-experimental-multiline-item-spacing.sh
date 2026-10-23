#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: format spaces multi-line items only with MultilineItemSpacing"

input='module A = {
  let x = 1
}
let y = 2
let z = 3
'
expected='module A = {
  let x = 1
}

let y = 2
let z = 3'

# The formatter writes CRLF on Windows. $(...) drops the trailing newline, so
# compare against the input without it.
without=$(printf '%s' "$input" | "$REWATCH_EXECUTABLE" format --stdin .res)
status=$?
without=$(printf '%s' "$without" | tr -d '\r')
if [ $status -ne 0 ] || [ "$without" != "${input%$'\n'}" ]; then
  error "Format changed spacing without the experimental feature"
  echo "$without"
  exit 1
fi

cp rescript.json rescript.json.bak
node -e '
const fs=require("fs");
const j=JSON.parse(fs.readFileSync("rescript.json","utf8"));
j["experimental-features"]={MultilineItemSpacing:true};
fs.writeFileSync("rescript.json", JSON.stringify(j,null,2));
'

with=$(printf '%s' "$input" | "$REWATCH_EXECUTABLE" format --stdin .res)
status=$?
mv rescript.json.bak rescript.json
with=$(printf '%s' "$with" | tr -d '\r')

if [ $status -ne 0 ] || [ "$with" != "$expected" ]; then
  error "Format did not space multi-line items with MultilineItemSpacing"
  echo "$with"
  exit 1
fi

success "MultilineItemSpacing is opt-in for format"
