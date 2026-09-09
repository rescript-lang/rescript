import { spawnSync } from "node:child_process";
import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

export const root = dirname(fileURLToPath(import.meta.url));
const repository = resolve(root, "../../../..");
const bin = join(
  repository,
  "packages/@rescript",
  `${process.platform}-${process.arch}`,
  "bin",
);
export const rewatch =
  process.env.REWATCH_EXECUTABLE ?? join(bin, "rescript.exe");
export const config = JSON.parse(
  readFileSync(join(root, "rescript.json"), "utf8"),
);
export const env = {
  ...process.env,
  REWATCH_EXECUTABLE: rewatch,
  RESCRIPT_BSC_EXE: process.env.RESCRIPT_BSC_EXE ?? join(bin, "bsc.exe"),
  RESCRIPT_RUNTIME:
    process.env.RESCRIPT_RUNTIME ??
    join(repository, "packages/@rescript/runtime"),
};

export function build() {
  const result = spawnSync(rewatch, ["build"], {
    cwd: root,
    env,
    stdio: "inherit",
  });
  if (result.error) throw result.error;
  if (result.status !== 0)
    throw new Error(`Rewatch build failed (${result.status})`);
}

if (
  process.argv[1] &&
  import.meta.url === pathToFileURL(resolve(process.argv[1])).href
)
  build();
