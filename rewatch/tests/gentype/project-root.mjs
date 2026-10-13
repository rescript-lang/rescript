import assert from "node:assert/strict";
import {spawnSync} from "node:child_process";
import {existsSync, mkdtempSync, mkdirSync, readFileSync, realpathSync, rmSync, symlinkSync, writeFileSync} from "node:fs";
import {tmpdir} from "node:os";
import {join} from "node:path";

const executable = process.env.REWATCH_EXECUTABLE;
assert(executable, "REWATCH_EXECUTABLE must be set");

function runBuild(buildDirectory) {
  if (process.platform !== "win32") {
    return spawnSync(executable, ["build", buildDirectory], {
      cwd: tmpdir(),
      encoding: "utf8",
      env: process.env,
      timeout: 30_000,
    });
  }

  const executablePath = executable.endsWith(".exe") ? executable : `${executable}.exe`;
  const commandShim = executable.endsWith(".cmd") ? executable : `${executable}.cmd`;
  const resolvedExecutable = existsSync(executablePath)
    ? executablePath
    : existsSync(commandShim)
      ? commandShim
      : executable;

  return spawnSync(
    process.env.ComSpec ?? "cmd.exe",
    [
      "/d",
      "/c",
      'cd /d "%REWATCH_TEST_PROJECT%" && "%REWATCH_TEST_EXECUTABLE%" build',
    ],
    {
      encoding: "utf8",
      env: {
        ...process.env,
        REWATCH_TEST_EXECUTABLE: resolvedExecutable,
        REWATCH_TEST_PROJECT: buildDirectory,
      },
      timeout: 30_000,
      windowsVerbatimArguments: true,
    },
  );
}

function getBuildDirectory(projectDir) {
  if (process.platform !== "win32") {
    const symlink = `${projectDir}-link`;
    symlinkSync(projectDir, symlink, "dir");
    return symlink;
  }

  const expandedPath = realpathSync.native(projectDir);
  const result = spawnSync(
    process.env.ComSpec ?? "cmd.exe",
    ["/d", "/c", 'for %I in ("%REWATCH_TEST_PROJECT%") do @echo %~sI'],
    {
      encoding: "utf8",
      env: {...process.env, REWATCH_TEST_PROJECT: expandedPath},
      windowsVerbatimArguments: true,
    },
  );
  assert.ifError(result.error);
  assert.equal(result.status, 0, result.stderr);

  const shortPath = result.stdout.trim();
  assert(shortPath, "Windows did not return a short path for the temporary project");
  if (shortPath.toLowerCase() === expandedPath.toLowerCase()) {
    console.log("SKIP: Windows 8.3 short names are unavailable on this volume");
    return null;
  }
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
  if (buildDirectory !== null) {
    assert(existsSync(buildDirectory), `Build directory does not exist: ${buildDirectory}`);
    const result = runBuild(buildDirectory);

    assert.ifError(result.error);
    assert.equal(result.status, 0, `${result.stdout}\n${result.stderr}`);
    assert.match(
      readFileSync(join(projectDir, "src-b", "nested", "B.gen.tsx"), "utf8"),
      /from '\.\.\/\.\.\/src-a\/A\.gen'/,
    );
  }
} finally {
  rmSync(`${projectDir}-link`, {force: true});
  rmSync(projectDir, {recursive: true, force: true});
}
