// @ts-check

import { setup } from "#dev/process";

const { execBuildLegacy } = setup(import.meta.dirname);

await execBuildLegacy();
