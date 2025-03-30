import * as assert from "node:assert";
import { setup } from "#dev/process";

const { execClean, execBuild } = setup(import.meta.dirname);

await execClean();
await execBuild();

const x = await import("./src/demo.res.js");
assert.equal(x.v, 42);
