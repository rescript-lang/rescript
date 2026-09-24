import assert from "node:assert/strict";
import { spawn, spawnSync } from "node:child_process";
import { existsSync } from "node:fs";
import { rm } from "node:fs/promises";
import path from "node:path";

const launcher = path.resolve("node_modules/rescript/cli/rescript.js");
const output = path.resolve("src/Test.mjs");
const watchLock = path.resolve("lib/watch.lock");
const timeoutMs = process.platform === "win32" ? 120000 : 30000;

function run(args, input) {
  const result = spawnSync(process.execPath, [launcher, ...args], {
    input,
    encoding: "utf8",
    timeout: timeoutMs,
  });
  if (result.error) throw result.error;
  assert.equal(result.status, 0, `${args.join(" ")} failed: ${result.stderr}`);
  return result.stdout;
}

async function waitForExit(promise) {
  let timer;
  try {
    return await Promise.race([
      promise,
      new Promise((_, reject) => {
        timer = setTimeout(
          () => reject(new Error("watch did not exit after shutdown request")),
          timeoutMs,
        );
      }),
    ]);
  } finally {
    clearTimeout(timer);
  }
}

async function waitUntil(predicate, message) {
  for (let attempt = 0; attempt < timeoutMs / 250; attempt++) {
    if (predicate()) return;
    await new Promise(resolve => setTimeout(resolve, 250));
  }
  throw new Error(message);
}

const formatted = run(["format", "--stdin", ".res"], "let x=1\n");
assert.match(formatted, /let x = 1/);

assert.ok(existsSync(output), "installation test should have built Test.mjs");
run(["clean"]);
assert.ok(!existsSync(output), "clean should remove Test.mjs");

const watcher = spawn(process.execPath, [launcher, "watch"], {
  stdio: ["ignore", "pipe", "pipe"],
});
let watchOutput = "";
watcher.stdout.on("data", chunk => {
  watchOutput += chunk;
});
watcher.stderr.on("data", chunk => {
  watchOutput += chunk;
});
const watcherExit = new Promise((resolve, reject) => {
  watcher.once("error", reject);
  watcher.once("exit", (code, signal) => resolve({ code, signal }));
});

try {
  await waitUntil(
    () => existsSync(watchLock) && existsSync(output),
    `watch did not build the project: ${watchOutput}`,
  );
  if (process.platform === "win32") {
    // Windows cannot deliver POSIX SIGINT to a child process through kill().
    await rm(watchLock);
  } else {
    assert.ok(watcher.kill("SIGINT"), "could not signal packaged CLI");
  }
  await waitForExit(watcherExit);
  await waitUntil(
    () => !existsSync(watchLock),
    `watch lock remained after shutdown: ${watchOutput}`,
  );
  assert.match(
    watchOutput,
    /Exiting\.\.\./,
    "watcher did not report a clean shutdown",
  );
} finally {
  await rm(watchLock, { force: true });
  if (watcher.exitCode === null && watcher.signalCode === null)
    watcher.kill("SIGTERM");
}
