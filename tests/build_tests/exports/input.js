// @ts-check

import { setup } from "#dev/process";

const { execBuild } = setup(import.meta.dirname);

await execBuild();
