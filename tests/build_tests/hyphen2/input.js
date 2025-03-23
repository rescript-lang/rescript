// @ts-check

import { setupWithUrl } from "#dev/process";

const { execBuild } = setupWithUrl(import.meta.url);

await execBuild();
