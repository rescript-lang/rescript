import child_process from "node:child_process";
// Vitest uses Vite's module resolution which doesn't support Node.js subpath
// imports (#cli/bins). We resolve the binary path directly instead.
import { bsc_exe, rescript_exe, runtimePath } from "./bins.mjs";

/**
 * Create a rescript CLI helper scoped to a working directory.
 * Provides methods to run rescript commands and spawn long-lived processes.
 *
 * @param {string} cwd - Working directory for spawned processes
 */
export function createRescriptCli(cwd) {
  const env = {
    ...process.env,
    RESCRIPT_BSC_EXE: bsc_exe,
    RESCRIPT_RUNTIME: runtimePath,
  };

  return {
    /**
     * Run `rescript build` and wait for completion.
     * @param {string[]} args
     * @returns {Promise<{status: number, stdout: string, stderr: string}>}
     */
    build(args = []) {
      return run(rescript_exe, ["build", ...args], { cwd, env });
    },

    /**
     * Run `rescript clean` and wait for completion.
     * @param {string[]} args
     * @returns {Promise<{status: number, stdout: string, stderr: string}>}
     */
    clean(args = []) {
      return run(rescript_exe, ["clean", ...args], { cwd, env });
    },

    /**
     * Run `rescript format` and wait for completion.
     * @param {string[]} args
     * @returns {Promise<{status: number, stdout: string, stderr: string}>}
     */
    format(args = []) {
      return run(rescript_exe, ["format", ...args], { cwd, env });
    },

    /**
     * Run `rescript format --stdin <ext>` with input piped to stdin.
     * @param {string} ext - File extension (.res or .resi)
     * @param {string} input - Source code to format
     * @returns {Promise<{status: number, stdout: string, stderr: string}>}
     */
    formatStdin(ext, input) {
      return runWithStdin(rescript_exe, ["format", "--stdin", ext], input, {
        cwd,
        env,
      });
    },

    /**
     * Run `rescript compiler-args <file>` and wait for completion.
     * @param {string} filePath
     * @returns {Promise<{status: number, stdout: string, stderr: string}>}
     */
    compilerArgs(filePath) {
      return run(rescript_exe, ["compiler-args", filePath], { cwd, env });
    },

    /**
     * Spawn `rescript watch` and return a handle with a `stop()` method.
     * Also exposes `stdout`, `stderr`, and the raw `process` for advanced use.
     * @param {string[]} args
     * @returns {{stop: () => void, stdout: import("stream").Readable, stderr: import("stream").Readable, process: child_process.ChildProcess}}
     */
    spawnWatch(args = []) {
      const proc = child_process.spawn(rescript_exe, ["watch", ...args], {
        cwd,
        stdio: ["ignore", "pipe", "pipe"],
        env,
      });
      return {
        stop() {
          proc.kill("SIGKILL");
        },
        get stdout() {
          return proc.stdout;
        },
        get stderr() {
          return proc.stderr;
        },
        process: proc,
      };
    },
  };
}

/**
 * @param {string} command
 * @param {string[]} args
 * @param {child_process.SpawnOptions} options
 * @returns {Promise<{status: number, stdout: string, stderr: string}>}
 */
function run(command, args, options) {
  return new Promise((resolve, reject) => {
    const stdoutChunks = [];
    const stderrChunks = [];

    const proc = child_process.spawn(command, args, {
      ...options,
      stdio: ["ignore", "pipe", "pipe"],
    });

    proc.stdout.on("data", chunk => stdoutChunks.push(chunk));
    proc.stderr.on("data", chunk => stderrChunks.push(chunk));

    proc.once("error", reject);
    proc.once("close", exitCode => {
      resolve({
        status: exitCode ?? 1,
        stdout: Buffer.concat(stdoutChunks).toString("utf8"),
        stderr: Buffer.concat(stderrChunks).toString("utf8"),
      });
    });
  });
}

/**
 * Like run() but pipes input to the process's stdin.
 * @param {string} command
 * @param {string[]} args
 * @param {string} input
 * @param {child_process.SpawnOptions} options
 * @returns {Promise<{status: number, stdout: string, stderr: string}>}
 */
function runWithStdin(command, args, input, options) {
  return new Promise((resolve, reject) => {
    const stdoutChunks = [];
    const stderrChunks = [];

    const proc = child_process.spawn(command, args, {
      ...options,
      stdio: ["pipe", "pipe", "pipe"],
    });

    proc.stdout.on("data", chunk => stdoutChunks.push(chunk));
    proc.stderr.on("data", chunk => stderrChunks.push(chunk));

    proc.once("error", reject);
    proc.once("close", exitCode => {
      resolve({
        status: exitCode ?? 1,
        stdout: Buffer.concat(stdoutChunks).toString("utf8"),
        stderr: Buffer.concat(stderrChunks).toString("utf8"),
      });
    });

    proc.stdin.write(input);
    proc.stdin.end();
  });
}
