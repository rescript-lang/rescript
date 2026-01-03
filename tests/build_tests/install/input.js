// @ts-check

import * as assert from "node:assert";
import { existsSync } from "node:fs";
import * as path from "node:path";
import { setup } from "#dev/process";

const { execBuildLegacy, execCleanLegacy } = setup(import.meta.dirname);

await execCleanLegacy();
await execBuildLegacy(["-install"]);

let fooExists = existsSync(path.join("lib", "ocaml", "Foo.cmi"));
assert.ok(!fooExists);

await execBuildLegacy();
await execBuildLegacy(["-install"]);

fooExists = existsSync(path.join("lib", "ocaml", "Foo.cmi"));
assert.ok(fooExists);
