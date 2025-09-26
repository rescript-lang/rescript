#!/bin/bash
cd $(dirname $0)
source "./utils.sh"
cd ../testrepo

bold "Test: experimental-features in rescript.json emits -enable-experimental as string list"

# Backup rescript.json
cp rescript.json rescript.json.bak

# Inject experimental-features enabling LetUnwrap using node for portability
node -e '
const fs=require("fs");
const j=JSON.parse(fs.readFileSync("rescript.json","utf8"));
j["experimental-features"]={LetUnwrap:true};
fs.writeFileSync("rescript.json", JSON.stringify(j,null,2));
'

stdout=$(rewatch compiler-args packages/file-casing/src/Consume.res 2>/dev/null)
if [ $? -ne 0 ]; then
  mv rescript.json.bak rescript.json
  error "Error grabbing compiler args with experimental-features enabled"
  exit 1
fi

# Expect repeated string-list style: two "-enable-experimental" entries and "LetUnwrap" present
enable_count=$(echo "$stdout" | grep -o '"-enable-experimental"' | wc -l | xargs)
echo "$stdout" | grep -q '"LetUnwrap"'
letunwrap_ok=$?
if [ "$enable_count" -ne 2 ] || [ $letunwrap_ok -ne 0 ]; then
  mv rescript.json.bak rescript.json
  error "Expected two occurrences of -enable-experimental and presence of LetUnwrap in compiler-args output"
  echo "$stdout"
  exit 1
fi

# Restore original rescript.json
mv rescript.json.bak rescript.json

success "experimental-features emits -enable-experimental twice as string list"
