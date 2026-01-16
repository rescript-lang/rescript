// @ts-check

import { setup } from "#dev/process";

const { execBuild, execClean } = setup(import.meta.dirname);

await execBuild();
await execClean();
