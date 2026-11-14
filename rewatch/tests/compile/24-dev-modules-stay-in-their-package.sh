#!/bin/bash
# A namespaced package's dev modules are aliased by its dev map, which only
# its own dev modules open: they neither shadow a consumer's modules after
# `open Provider` nor are reachable from other packages or non-dev modules.

cd "$(dirname "$0")"
source "../utils.sh"

bold "Test: Dev modules stay in their package"

test_dir=$(pwd)
fixture=$(mktemp -d 2>/dev/null || mktemp -d -t rewatch-dev-modules)
trap 'cd "$test_dir"; rm -rf "$fixture"' EXIT
cd "$fixture"

node <<'NODE'
const fs = require("fs");
const path = require("path");
const write = (file, contents) => {
  fs.mkdirSync(path.dirname(file), {recursive: true});
  fs.writeFileSync(file, contents);
};
const json = value => JSON.stringify(value, null, 2) + "\n";
const specs = {
  "package-specs": {module: "esmodule", "in-source": true},
  suffix: ".mjs",
};
write("package.json", json({name: "root", private: true, workspaces: ["consumer", "provider"]}));
write("rescript.json", json({name: "root", sources: ["src"], dependencies: ["consumer"], ...specs}));
write("src/Main.res", "let value = Consumer.Use.value\n");
for (const name of ["consumer", "provider"]) {
  write(`${name}/package.json`, json({name, version: "1.0.0"}));
  write(`${name}/rescript.json`, json({
    name,
    namespace: true,
    sources: [{dir: "src"}, {dir: "test", type: "dev"}],
    dependencies: name === "consumer" ? ["provider"] : [],
    ...specs,
  }));
  fs.mkdirSync("node_modules", {recursive: true});
  // A junction on Windows, which needs no privileges; a symlink elsewhere.
  fs.symlinkSync(path.resolve(name), `node_modules/${name}`, "junction");
}
write("provider/src/Public.res", "let make = () => 1\n");
write("provider/test/Config.res", "let other = 2\n");
// A dev module reaches its package's dev modules without qualification.
write("provider/test/Local.res", "let value = Config.other + Public.make()\n");
write("consumer/src/Use.res", "let value = Provider.Public.make()\n");
write("consumer/test/Config.res", "let get = () => 3\n");
// Opening Provider must not shadow the consumer's own Config (#9089).
write("consumer/test/Check.res", "open Provider\nlet value = Config.get() + Public.make()\n");
NODE

# Usage: build_succeeds <message> [build options]
build_succeeds() {
  local message=$1
  shift
  if ! output=$(rewatch build "$@" 2>&1); then
    error "$message"
    printf '%s\n' "$output" >&2
    exit 1
  fi
}

# Usage: build_fails <message> <expected error>
build_fails() {
  if output=$(rewatch build 2>&1); then
    error "$1"
    exit 1
  fi
  if ! printf '%s\n' "$output" | grep -q "$2"; then
    error "$1: unexpected error"
    printf '%s\n' "$output" >&2
    exit 1
  fi
}

build_succeeds "Opening a dependency's namespace let its dev modules shadow the consumer's"

# The new dev module changes the dev map. Local, compiled again, then reads
# Config's interface, compiled against the previous dev map: the dev map's
# interface must not be recorded as an import.
printf 'let value = 4\n' > provider/test/Extra.res
build_succeeds "Adding a dev module failed"
printf 'let value = Config.other + Public.make() + 1\n' > provider/test/Local.res
build_succeeds "A dev module compiled against the previous dev map broke a dependent"

printf 'let value = Provider.Config.other\n' > consumer/test/Leak.res
build_fails "A dependency's dev module was reachable through its namespace" \
  "Provider.Config can't be found"
rm consumer/test/Leak.res

printf 'let value = Config.other\n' > provider/src/Bad.res
build_fails "A non-dev module reached a dev module of its package" \
  "Config can't be found"
rm provider/src/Bad.res
build_succeeds "Build failed after removing the invalid modules"

build_succeeds "Production build failed" --prod
if [ -e provider/lib/rescript/Provider-dev.cmi ]; then
  error "The production build kept the dev map"
  exit 1
fi

# With a namespace entry, a dev test can use the entry: the namespace map no
# longer depends on the dev modules, so this is not a dependency cycle.
node <<'NODE'
const fs = require("fs");
const config = JSON.parse(fs.readFileSync("provider/rescript.json", "utf8"));
config["namespace-entry"] = "Entry";
fs.writeFileSync("provider/rescript.json", JSON.stringify(config, null, 2) + "\n");
fs.writeFileSync("provider/src/Entry.res", "module Public = Public\n");
fs.writeFileSync("provider/test/Local.res", "let value = Config.other + Entry.Public.make()\n");
fs.writeFileSync("consumer/src/Use.res", "let value = Entry.Public.make()\n");
fs.writeFileSync("consumer/test/Check.res", "open Entry\nlet value = Config.get() + Public.make()\n");
NODE
rewatch clean &> /dev/null
build_succeeds "A dev test using the namespace entry failed to build"

success "Dev modules stay in their package"
