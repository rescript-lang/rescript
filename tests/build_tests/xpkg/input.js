// @ts-check

import * as assert from "node:assert";
import { setupWithUrl } from "#dev/process";

const { execBuild } = await setupWithUrl(import.meta.url);

const output = await execBuild(["-regen"]);
assert.match(output.stderr, /reserved package name/);
