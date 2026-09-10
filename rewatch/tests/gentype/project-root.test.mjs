import assert from "node:assert/strict";
import {spawnSync} from "node:child_process";
import {mkdtempSync, mkdirSync, readFileSync, rmSync, writeFileSync} from "node:fs";
import {tmpdir} from "node:os";
import {join} from "node:path";

const executable = process.env.REWATCH_EXECUTABLE;
assert(executable, "REWATCH_EXECUTABLE must be set");

const projectDir = mkdtempSync(join(tmpdir(), "rescript-gentype-root-"));

try {
  mkdirSync(join(projectDir, "src"));
  writeFileSync(
    join(projectDir, "rescript.json"),
    `${JSON.stringify({name: "gentype-project-root", sources: ["src"], gentypeconfig: {}}, null, 2)}\n`,
  );
  writeFileSync(join(projectDir, "src", "A.res"), "@genType\ntype t = {value: int}\n");
  writeFileSync(
    join(projectDir, "src", "B.res"),
    "@genType\nlet getValue = (record: A.t) => record.value\n",
  );

  const result = spawnSync(executable, ["build"], {
    cwd: projectDir,
    encoding: "utf8",
    env: process.env,
    timeout: 30_000,
  });

  assert.ifError(result.error);
  assert.equal(result.status, 0, `${result.stdout}\n${result.stderr}`);
  assert.match(readFileSync(join(projectDir, "src", "B.gen.tsx"), "utf8"), /A\.gen/);
} finally {
  rmSync(projectDir, {recursive: true, force: true});
}
