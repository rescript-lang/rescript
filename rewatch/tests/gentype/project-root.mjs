import assert from "node:assert/strict";
import {spawnSync} from "node:child_process";
import {existsSync, mkdtempSync, mkdirSync, readFileSync, realpathSync, rmSync, symlinkSync, writeFileSync} from "node:fs";
import {tmpdir} from "node:os";
import {join} from "node:path";

const executable = process.env.REWATCH_EXECUTABLE;
assert(executable, "REWATCH_EXECUTABLE must be set");

const executablePath = executable.endsWith(".exe") ? executable : `${executable}.exe`;
const commandShim = executable.endsWith(".cmd") ? executable : `${executable}.cmd`;
const resolvedExecutable = existsSync(executablePath)
  ? executablePath
  : existsSync(commandShim)
    ? commandShim
    : executable;

function runBuild(buildDirectory) {
  if (process.platform !== "win32") {
    return spawnSync(executable, ["build", buildDirectory], {
      cwd: tmpdir(),
      encoding: "utf8",
      env: process.env,
      timeout: 30_000,
    });
  }

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

function checkEditorCompilation(buildDirectory) {
  const options = {encoding: "utf8", timeout: 30_000};
  const result = spawnSync(
    resolvedExecutable,
    ["compiler-args", join(buildDirectory, "src-a", "A.res")],
    {...options, shell: resolvedExecutable.endsWith(".cmd")},
  );
  assert.ifError(result.error);
  assert.equal(result.status, 0, result.stderr);
  const {compiler_args: args} = JSON.parse(result.stdout);
  assert.equal(args[args.indexOf("-bs-project-root") + 1], buildDirectory);

  // The editor compiles an absolute incremental source instead of the AST,
  // and omits generated-JavaScript output while retaining the GenType flags.
  args.splice(args.indexOf("-bs-package-output"), 2);
  args.pop();
  const incrementalDir = join(buildDirectory, "lib", "bs", "___incremental");
  mkdirSync(incrementalDir);
  const source = join(incrementalDir, "Editor.res");
  writeFileSync(source, "@genType\nlet value = 42\n");
  const compiled = spawnSync(process.env.RESCRIPT_BSC_EXE, [...args, source], {
    ...options,
    cwd: join(buildDirectory, "lib", "bs"),
  });
  assert.ifError(compiled.error);
  assert.equal(compiled.status, 0, `${compiled.stdout}\n${compiled.stderr}`);
  assert(existsSync(join(incrementalDir, "Editor.gen.tsx")));
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
    checkEditorCompilation(buildDirectory);
  }
} finally {
  rmSync(`${projectDir}-link`, {force: true});
  rmSync(projectDir, {recursive: true, force: true});
}
