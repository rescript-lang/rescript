import * as assert from "node:assert";
import { setup } from "#dev/process";

const { execCleanLegacy, execBuildLegacy } = setup(import.meta.dirname);

await execCleanLegacy();
await execBuildLegacy();

const x = await import("./src/demo.res.js");
assert.equal(x.v, 42);
