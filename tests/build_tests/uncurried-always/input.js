// @ts-check

import { setup } from "#dev/process";

const { execBuildLegacy, execCleanLegacy } = setup(import.meta.dirname);

await execCleanLegacy();
await execBuildLegacy();
