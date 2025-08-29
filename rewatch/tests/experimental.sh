#!/bin/bash
cd $(dirname $0)
source "./utils.sh"
cd ../testrepo

bold "Test: experimentalFeatures in rescript.json emits -enable-experimental as string list"

# Backup rescript.json
cp rescript.json rescript.json.bak

# Inject experimentalFeatures enabling LetUnwrap using node for portability
node -e '
const fs=require("fs");
const j=JSON.parse(fs.readFileSync("rescript.json","utf8"));
j.experimentalFeatures={LetUnwrap:true};
fs.writeFileSync("rescript.json", JSON.stringify(j,null,2));
'

stdout=$(rewatch compiler-args packages/file-casing/src/Consume.res 2>/dev/null)
if [ $? -ne 0 ]; then
  mv rescript.json.bak rescript.json
  error "Error grabbing compiler args with experimentalFeatures enabled"
  exit 1
fi

# Expect repeated string-list style: presence of -enable-experimental and LetUnwrap entries
echo "$stdout" | grep -q '"-enable-experimental"' && echo "$stdout" | grep -q '"LetUnwrap"'
if [ $? -ne 0 ]; then
  mv rescript.json.bak rescript.json
  error "-enable-experimental / LetUnwrap not found in compiler-args output"
  echo "$stdout"
  exit 1
fi

# Restore original rescript.json
mv rescript.json.bak rescript.json

success "experimentalFeatures emits -enable-experimental as string list"
