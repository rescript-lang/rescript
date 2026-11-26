#!/bin/bash
set -e
cd "$(dirname "$0")"
source "../utils.sh"

bold "Test: Clean removes same-named modules from every package"

test_dir=$(pwd)
fixture=$(mktemp -d)
trap 'cd "$test_dir"; rm -rf "$fixture"' EXIT
cd "$fixture"

node <<'NODE'
const fs = require("fs");
const config = name => ({
  name,
  sources: ["src"],
  namespace: false,
  "package-specs": {module: "esmodule", "in-source": true},
  suffix: ".mjs",
});
const dependencies = ["clean-duplicate-a", "clean-duplicate-b"];
fs.mkdirSync("src");
fs.writeFileSync("src/Main.res", "let value = 0\n");
fs.writeFileSync("rescript.json", JSON.stringify({...config("clean-duplicates"), dependencies}));
for (const [index, name] of dependencies.entries()) {
  const directory = `node_modules/${name}`;
  fs.mkdirSync(`${directory}/src`, {recursive: true});
  fs.writeFileSync(`${directory}/package.json`, JSON.stringify({name, version: "1.0.0"}));
  fs.writeFileSync(`${directory}/rescript.json`, JSON.stringify(config(name)));
  fs.writeFileSync(`${directory}/src/Foo.res`, `let value = ${index}\n`);
}
NODE

# Build separately so both packages have real output before root-level cleanup.
for package in clean-duplicate-a clean-duplicate-b; do
  if ! output=$(cd "node_modules/$package" && rewatch build 2>&1); then
    error "Could not build $package"
    printf '%s\n' "$output" >&2
    exit 1
  fi
  if [ ! -f "node_modules/$package/src/Foo.mjs" ]; then
    error "Test setup did not produce $package/src/Foo.mjs"
    exit 1
  fi
done

if ! output=$(rewatch clean 2>&1); then
  error "Could not clean packages with same-named modules"
  printf '%s\n' "$output" >&2
  exit 1
fi

for package in clean-duplicate-a clean-duplicate-b; do
  if [ -f "node_modules/$package/src/Foo.mjs" ]; then
    error "Clean left $package/src/Foo.mjs behind"
    exit 1
  fi
done

success "Same-named modules cleaned from both packages"
