#!/bin/bash
set -e
cd "$(dirname "$0")"
source ../utils.sh

bold "Test: Internal compilation units belong to their defining project"
fixture=$(mktemp -d 2>/dev/null || mktemp -d -t rewatch-project-visibility)
trap 'rm -rf "$fixture"' EXIT

expect_internal_error() {
  local output
  if output=$("$REWATCH_EXECUTABLE" build "$consumer" 2>&1); then
    error "Consumer accessed an internal unit: $1"
    exit 1
  fi
  if ! printf '%s\n' "$output" | grep -q 'is internal to another ReScript project'; then
    error "Expected an access diagnostic: $1"
    printf '%s\n' "$output"
    exit 1
  fi
}

for mode in plain namespace entry; do
  provider="$fixture/$mode/provider"
  consumer="$fixture/$mode/consumer"
  mkdir -p "$provider/src" "$consumer/src"
  MODE="$mode" PROVIDER="$provider" CONSUMER="$consumer" node <<'JS'
const fs = require("node:fs");
const path = require("node:path");
const { MODE, PROVIDER, CONSUMER } = process.env;
const write = (root, name, value) => fs.writeFileSync(path.join(root, name), JSON.stringify(value));
write(PROVIDER, "package.json", {name: "visibility-provider", version: "1.0.0", files: ["src", "rescript.json"]});
const config = {name: "visibility-provider", namespace: MODE === "plain" ? false : "Provider", sources: {dir: "src", public: ["Public"]}};
if (MODE === "entry") config["namespace-entry"] = "Public";
write(PROVIDER, "rescript.json", config);
write(CONSUMER, "package.json", {name: "visibility-consumer", private: true});
write(CONSUMER, "rescript.json", {name: "visibility-consumer", sources: ["src"], dependencies: ["visibility-provider"]});
JS
  cat > "$provider/src/Helper.res" <<'RES'
type native = {value: int}
let secret = 7
let make = () => {value: secret}
module Impl = (R: {type t}) => {
  @send external run: R.t => native = "run"
}
RES
  cat > "$provider/src/Helper.resi" <<'RES'
type native = private {value: int}
let secret: int
let make: unit => native
module Impl: (R: {type t}) => {
  @send external run: R.t => native = "run"
}
RES
  cat > "$provider/src/Public.res" <<'RES'
type t = Helper.native
let value = Helper.secret
let make = Helper.make
module Reexport = Helper
module Safe = Helper
module Impl = Helper.Impl
RES
  cat > "$provider/src/Public.resi" <<'RES'
type t = Helper.native
let value: int
let make: unit => t
module Safe: {let secret: int}
module Reexport = Helper
RES
  cat > "$provider/src/Peer.res" <<'RES'
module Receiver = {type t = {run: unit => Helper.native}}
module Applied = Helper.Impl(Receiver)
let result = Applied.run({run: () => Helper.make()})
RES
  # A source-only tarball must retain helpers for dependency-side compilation.
  (cd "$provider" && npm pack --ignore-scripts --json > pack.json)
  (cd "$consumer" && npm install --ignore-scripts --no-audit --no-fund \
    "$provider/visibility-provider-1.0.0.tgz" > install.log 2>&1)
  prefix=""
  if [ "$mode" = namespace ]; then prefix="Provider."; fi
  printf 'let x: int = %sPublic.value\nlet y: int = %sPublic.Safe.secret\n' "$prefix" "$prefix" > "$consumer/src/Main.res"
  printf 'let instance: %sPublic.t = %sPublic.make()\nlet field: int = instance.value\n' "$prefix" "$prefix" >> "$consumer/src/Main.res"
  "$REWATCH_EXECUTABLE" build "$consumer"

  # Public aliases retain the target's ownership unless a signature seals it.
  printf 'let x = %sPublic.Reexport.secret\n' "$prefix" > "$consumer/src/Main.res"
  expect_internal_error "$mode: public module alias"
  if [ "$mode" = entry ]; then
    # The internal namespace map must not be an escape hatch via -open.
    CONFIG="$consumer/rescript.json" node <<'JS'
const fs = require("node:fs");
const config = JSON.parse(fs.readFileSync(process.env.CONFIG));
config["compiler-flags"] = ["-open", "@Provider"];
fs.writeFileSync(process.env.CONFIG, JSON.stringify(config));
JS
  fi
  printf 'let x = %sHelper.secret\n' "$prefix" > "$consumer/src/Main.res"
  expect_internal_error "$mode: helper value"
  printf 'module Alias = %sHelper\n' "$prefix" > "$consumer/src/Main.res"
  expect_internal_error "$mode: module alias"
  printf 'include %sHelper\n' "$prefix" > "$consumer/src/Main.res"
  expect_internal_error "$mode: include"
  printf 'module R = {type t = int}\nmodule F = %sHelper.Impl(R)\n' "$prefix" > "$consumer/src/Main.res"
  expect_internal_error "$mode: functor application"
  if [ "$mode" = namespace ]; then
    printf 'open Provider\nlet x = Helper.secret\n' > "$consumer/src/Main.res"
    expect_internal_error "$mode: opened namespace"
  fi
  # Changing public to all invalidates same-signature interfaces and consumers.
  CONFIG="$consumer/node_modules/visibility-provider/rescript.json" node <<'JS'
const fs = require("node:fs");
const config = JSON.parse(fs.readFileSync(process.env.CONFIG));
config.sources.public = "all";
fs.writeFileSync(process.env.CONFIG, JSON.stringify(config));
JS
  printf 'let x = %sPublic.Reexport.secret\n' "$prefix" > "$consumer/src/Main.res"
  "$REWATCH_EXECUTABLE" build "$consumer"
done

success "Packed helpers are accessible to siblings, not consuming projects"

# A root build must not turn sibling workspace packages into one owner.
workspace="$fixture/workspace"
provider="$workspace/packages/provider"
consumer="$workspace/packages/consumer"
mkdir -p "$provider/src" "$provider/test" "$consumer/src" "$workspace/node_modules"
WORKSPACE="$workspace" PROVIDER="$provider" CONSUMER="$consumer" node <<'JS'
const fs = require("node:fs");
const path = require("node:path");
const { WORKSPACE, PROVIDER, CONSUMER } = process.env;
const write = (root, file, value) => fs.writeFileSync(path.join(root, file), JSON.stringify(value));
write(WORKSPACE, "rescript.json", {name: "workspace", sources: [], dependencies: ["provider", "consumer"]});
write(PROVIDER, "rescript.json", {name: "provider", namespace: "Provider", sources: [{dir: "src", public: ["Public"]}, {dir: "test", type: "dev"}]});
write(CONSUMER, "rescript.json", {name: "consumer", namespace: "Consumer", sources: ["src"], dependencies: ["provider"]});
for (const [name, target] of [["provider", PROVIDER], ["consumer", CONSUMER]]) {
  fs.symlinkSync(target, path.join(WORKSPACE, "node_modules", name), "junction");
}
JS
printf 'let secret = 7\n' > "$provider/src/Helper.res"
printf 'let value = Helper.secret\n' > "$provider/src/Public.res"
printf 'let value = Helper.secret\n' > "$provider/test/OwnTest.res"
printf 'let value = Provider.Public.value\n' > "$consumer/src/Main.res"
"$REWATCH_EXECUTABLE" build "$workspace"
"$REWATCH_EXECUTABLE" build "$consumer"
printf 'let value = Provider.Helper.secret\n' > "$consumer/src/Main.res"
if output=$("$REWATCH_EXECUTABLE" build "$workspace" 2>&1); then
  error "A workspace root build granted cross-project internal access"
  exit 1
fi
if ! printf '%s\n' "$output" | grep -q 'is internal to another ReScript project'; then
  error "Workspace build did not report an internal-access error"
  printf '%s\n' "$output"
  exit 1
fi
expect_internal_error "leaf workspace build"
printf 'let value = Provider.Public.value\n' > "$consumer/src/Main.res"
"$REWATCH_EXECUTABLE" build --prod "$workspace"
success "Root, leaf and production builds preserve project ownership"
