#!/usr/bin/env bash
set -euo pipefail

: "${REWATCH_EXECUTABLE:?Set REWATCH_EXECUTABLE}"
project=$(mktemp -d)
trap 'rm -rf "$project"' EXIT

node - "$project" <<'JS'
const fs = require('fs');
const path = require('path');
const root = process.argv[2];
function write(file, value) {
  const target = path.join(root, file);
  fs.mkdirSync(path.dirname(target), {recursive: true});
  fs.writeFileSync(target, typeof value === 'string' ? value : JSON.stringify(value));
}
write('package.json', {name: 'root', private: true, workspaces: ['consumer', 'provider']});
write('rescript.json', {name: 'root', sources: ['src'], dependencies: ['consumer']});
write('src/Main.res', 'let value = Consumer.Use.value\n');
for (const name of ['consumer', 'provider']) {
  write(name + '/package.json', {name, version: '1.0.0'});
  write(name + '/rescript.json', {
    name, namespace: true,
    sources: [{dir: 'src'}, {dir: 'test', type: 'dev'}],
    dependencies: name === 'consumer' ? ['provider'] : [],
    warnings: {number: '-33-44'},
  });
  fs.mkdirSync(root + '/node_modules', {recursive: true});
  fs.symlinkSync('../' + name, root + '/node_modules/' + name);
}
write('provider/src/Public.res', 'type t = {value: int}\nlet make = () => {value: 1}\n');
write('provider/src/Alias.res', 'module Public = Public\nlet make = Public.make\n');
write('provider/src/Alias.resi', 'module Public = Public\nlet make: unit => Public.t\n');
write('provider/test/Config.res', 'let other = 2\n');
write('provider/test/Local.res', 'let value = Config.other\nlet qualified = Provider.Config.other\n');
write('consumer/src/Use.res', 'let value = Provider.Alias.make()\n');
write('consumer/test/Config.res', 'let get = () => 3\n');
write('consumer/test/Check.res', 'open Provider\nlet value = Config.get()\nlet qualified = Consumer.Config.get()\n');
JS

cd "$project"
"$REWATCH_EXECUTABLE" build
"$REWATCH_EXECUTABLE" build

# Simulate a namespace published by an older rewatch, without changing sources.
for extension in mlmap cmi cmt cmj; do
  cp "provider/lib/bs/Provider.$extension" "provider/lib/ocaml/Provider.$extension"
done
"$REWATCH_EXECUTABLE" build
if grep -qx Config provider/lib/ocaml/Provider.mlmap; then
  echo 'Stale dependency dev alias was not removed' >&2
  exit 1
fi

# A dependency's dev module must not be accessible through its namespace.
printf 'let value = Provider.Config.other\n' > consumer/test/Leak.res
if "$REWATCH_EXECUTABLE" build > leak.log 2>&1; then
  echo 'Dependency dev module was visible to a consumer' >&2
  exit 1
fi
grep -E 'Config.*(found|unbound)|module or file Config' leak.log
rm consumer/test/Leak.res
"$REWATCH_EXECUTABLE" build

echo 'Dev-module namespace isolation passed'

# Namespace entries must expose interfaces compatible with the public namespace.
node <<'NODE'
const fs = require('fs');
const config = JSON.parse(fs.readFileSync('provider/rescript.json', 'utf8'));
config['namespace-entry'] = 'Entry';
fs.writeFileSync('provider/rescript.json', JSON.stringify(config));
fs.writeFileSync('provider/src/Entry.res', 'module Public = Public\n');
fs.writeFileSync('provider/test/Local.res', 'let value = Config.other\n');
fs.writeFileSync('consumer/src/Use.res', 'let value = Entry.Public.make()\n');
fs.writeFileSync('consumer/test/Check.res', 'let value = Config.get()\n');
NODE
"$REWATCH_EXECUTABLE" clean
"$REWATCH_EXECUTABLE" build
"$REWATCH_EXECUTABLE" build

echo 'Namespace-entry isolation passed'
