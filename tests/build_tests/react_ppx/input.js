// @ts-check

import { setup } from "#dev/process";

const { execBuildOrThrow } = setup(import.meta.dirname);

await execBuildOrThrow();
