// @ts-check

import { setup } from "#dev/process";

const { execBuild, tsc } = setup(import.meta.dirname);

if (process.platform === "win32") {
  console.log("Skipping test on Windows");
  process.exit(0);
}

await execBuild(["-with-deps"]);
await tsc();
