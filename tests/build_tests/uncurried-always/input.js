// @ts-check

import { setupWithUrl } from "#dev/process";

const { execBuild, execClean } = setupWithUrl(import.meta.url);

await execClean();
await execBuild();
