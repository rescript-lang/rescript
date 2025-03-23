import * as child_process from "node:child_process";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { bsc_exe, cliPath, rescript_exe } from "#cli/paths";

/**
 * @typedef {{
 *   status: number,
 *   stdout: string,
 *   stderr: string,
 * }} ExecResult
 */

const signals = {
  SIGINT: 2,
  SIGQUIT: 3,
  SIGKILL: 9,
  SIGTERM: 15,
};

export const { exec, node, npx, mocha, bsc, rescript, execBuild, execClean } =
  setup();

/**
 * @param {string} url
 */
export function setupWithUrl(url) {
  return setup(path.dirname(fileURLToPath(url)));
}

/**
 * @param {string} [cwd]
 */
export function setup(cwd = process.cwd()) {

  /**
   * @param {string} command
   * @param {string[]} [args]
   * @param {child_process.SpawnOptions} [options]
   * @return {Promise<ExecResult>}
   */
  async function exec(command, args = [], options = {}) {
    const stdoutChunks = [];
    const stderrChunks = [];

    const subprocess = child_process.spawn(command, args, {
      cwd,
      shell: process.platform === "win32",
      stdio: ["ignore", "pipe", "pipe"],
      ...options,
    });

    subprocess.stdout?.on("data", chunk => {
      stdoutChunks.push(chunk);
    });

    subprocess.stderr?.on("data", chunk => {
      stderrChunks.push(chunk);
    });

    return await new Promise((resolve, reject) => {
      subprocess.once("error", err => {
        reject(err);
      });

      subprocess.once("close", (exitCode, signal) => {
        const stdout = Buffer.concat(stdoutChunks).toString("utf8");
        const stderr = Buffer.concat(stderrChunks).toString("utf8");

        let code = exitCode ?? 1;
        if (signals[signal]) {
          // + 128 is standard POSIX practice, see also https://nodejs.org/api/process.html#exit-codes
          code = signals[signal] + 128;
        }

        resolve({ status: code, stdout, stderr });
      });
    });
  }

  return {
    exec,

    /**
     * `node` CLI
     *
     * @param {string[]} [args]
     * @param {child_process.SpawnOptions} [options]
     * @return {Promise<ExecResult>}
     */
    node(args = [], options = {}) {
      return exec("node", args, options);
    },

    /**
     * `npx` CLI
     *
     * @param {string[]} [args]
     * @param {child_process.SpawnOptions} [options]
     * @return {Promise<ExecResult>}
     */
    npx(args = [], options = {}) {
      return exec("npx", args, options);
    },

    /**
     * Mocha CLI
     *
     * @param {string[]} [args]
     * @param {child_process.SpawnOptions} [options]
     * @return {Promise<ExecResult>}
     */
    mocha(args = [], options = {}) {
      return exec("npx", ["mocha", ...args], options);
    },

    /**
     * `rescript` CLI
     *
     * @param {(
     *   | "build"
     *   | "clean"
     *   | "format"
     *   | "dump"
     *   | (string & {})
     * )} command
     * @param {string[]} [args]
     * @param {child_process.SpawnOptions} [options]
     * @return {Promise<ExecResult>}
     */
    rescript(command, args = [], options = {}) {
      return exec(cliPath, [command, ...args].filter(Boolean), options);
    },

    /**
     * `bsc` CLI
     *
     * @param {string[]} [args]
     * @param {child_process.SpawnOptions} [options]
     * @return {Promise<ExecResult>}
     */
    bsc(args = [], options = {}) {
      return exec(bsc_exe, args, options);
    },

    /**
     * Execute ReScript `build` command directly
     *
     * @param {string[]} [args]
     * @param {child_process.SpawnOptions} [options]
     * @return {Promise<ExecResult>}
     */
    execBuild(args = [], options = {}) {
      return exec(rescript_exe, ["build", ...args], options);
    },

    /**
     * Execute ReScript `clean` command directly
     *
     * @param {string[]} [args]
     * @param {child_process.SpawnOptions} [options]
     * @return {Promise<ExecResult>}
     */
    execClean(args = [], options = {}) {
      return exec(rescript_exe, ["clean", ...args], options);
    },
  };
}
