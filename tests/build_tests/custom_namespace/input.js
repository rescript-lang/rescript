import * as assert from "node:assert";
import { setupWithUrl } from "#dev/process";

const { execClean, execBuild } = setupWithUrl(import.meta.url);

await execClean();
await execBuild();

const x = await import("./src/demo.res.js");
assert.equal(x.v, 42);
