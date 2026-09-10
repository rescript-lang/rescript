import assert from "node:assert/strict";
import {spawnSync} from "node:child_process";
import {existsSync, mkdtempSync, mkdirSync, readFileSync, realpathSync, rmSync, writeFileSync} from "node:fs";
import {tmpdir} from "node:os";
import {join} from "node:path";

const executable = process.env.REWATCH_EXECUTABLE;
assert(executable, "REWATCH_EXECUTABLE must be set");
const executableForNode =
  process.platform === "win32" && existsSync(`${executable}.cmd`) ? `${executable}.cmd` : executable;

function getBuildDirectory(projectDir) {
  if (process.platform !== "win32") {
    return projectDir;
  }

  const expandedPath = realpathSync.native(projectDir);
  const result = spawnSync(process.env.ComSpec ?? "cmd.exe", ["/d", "/s", "/c", `for %I in ("${expandedPath}") do @echo %~sI`], {
    encoding: "utf8",
  });
  assert.ifError(result.error);
  assert.equal(result.status, 0, result.stderr);

  const shortPath = result.stdout.trim();
  assert(shortPath, "Windows did not return a short path for the temporary project");
  assert.notEqual(
    shortPath.toLowerCase(),
    expandedPath.toLowerCase(),
    "Windows short and expanded project paths must differ for this regression",
  );
  return shortPath;
}

const projectDir = mkdtempSync(join(tmpdir(), "rescript-gentype-root-"));

try {
  mkdirSync(join(projectDir, "src-a"));
  mkdirSync(join(projectDir, "src-b", "nested"), {recursive: true});
  writeFileSync(
    join(projectDir, "rescript.json"),
    `${JSON.stringify(
      {
        name: "gentype-project-root",
        sources: ["src-a", {dir: "src-b", subdirs: true}],
        gentypeconfig: {},
      },
      null,
      2,
    )}\n`,
  );
  writeFileSync(join(projectDir, "src-a", "A.res"), "@genType\ntype t = {value: int}\n");
  writeFileSync(
    join(projectDir, "src-b", "nested", "B.res"),
    "@genType\nlet getValue = (record: A.t) => record.value\n",
  );

  const buildDirectory = getBuildDirectory(projectDir);
  const result = spawnSync(executableForNode, ["build"], {
    cwd: buildDirectory,
    encoding: "utf8",
    env: process.env,
    timeout: 30_000,
  });

  assert.ifError(result.error);
  assert.equal(result.status, 0, `${result.stdout}\n${result.stderr}`);
  assert.match(
    readFileSync(join(projectDir, "src-b", "nested", "B.gen.tsx"), "utf8"),
    /\.\.\/\.\.\/src-a\/A\.gen/,
  );
} finally {
  rmSync(projectDir, {recursive: true, force: true});
}
