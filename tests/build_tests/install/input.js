// @ts-check

import * as assert from "node:assert";
import { existsSync } from "node:fs";
import * as path from "node:path";
import { setup } from "#dev/process";

const { execBuild, execClean } = setup(import.meta.dirname);

await execClean();
await execBuild(["-install"]);

let fooExists = existsSync(path.join("lib", "ocaml", "Foo.cmi"));
assert.ok(!fooExists);

await execBuild();
await execBuild(["-install"]);

fooExists = existsSync(path.join("lib", "ocaml", "Foo.cmi"));
assert.ok(fooExists);
