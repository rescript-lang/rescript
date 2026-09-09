import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { env } from "./build.mjs";

const runner = fileURLToPath(
  new URL("../../platforms/acceptance.test.mjs", import.meta.url),
);
const result = spawnSync(process.execPath, ["--test", runner], {
  env,
  stdio: "inherit",
});
if (result.error) throw result.error;
process.exitCode = result.status ?? 1;
