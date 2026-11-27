// @ts-check

import { setup } from "#dev/process";

const { execBuildOrThrow, execClean } = setup(import.meta.dirname);

await execClean();
await execBuildOrThrow();
await execClean();
