import * as child_process from "node:child_process";
import * as path from "node:path";
import { bsc_exe, rescript_exe } from "#cli/bins";

/**
 * @typedef {{
 *   throwOnFail?: boolean,
 * } & child_process.SpawnOptions} ExecOptions
 *
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

export const {
  shell,
  node,
  yarn,
  mocha,
  bsc,
  rescript,
  execBin,
  execBuild,
  execClean,
} = setup();

/**
 * @param {string} [cwd]
 */
export function setup(cwd = process.cwd()) {
  /**
   * @param {string} command
   * @param {string[]} [args]
   * @param {ExecOptions} [options]
   * @return {Promise<ExecResult>}
   */
  async function exec(command, args = [], options = {}) {
    const { throwOnFail = options.stdio === "inherit" } = options;

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

        if (throwOnFail && code !== 0) {
          reject(
            new Error(
              `Command ${command} exited with non-zero status: ${code}`,
            ),
          );
        } else {
          resolve({ status: code, stdout, stderr });
        }
      });
    });
  }

  return {
    /**
     * bash shell script
     *
     * @param {string} script
     * @param {string[]} [args]
     * @param {ExecOptions} [options]
     * @return {Promise<ExecResult>}
     */
    shell(script, args = [], options = {}) {
      return exec("bash", [script, ...args], options);
    },

    /**
     * Execute JavaScript on Node.js
     *
     * @param {string} script
     * @param {string[]} [args]
     * @param {ExecOptions} [options]
     * @return {Promise<ExecResult>}
     */
    node(script, args = [], options = {}) {
      return exec("node", [script, ...args], options);
    },

    /**
     * Execute Yarn command
     *
     * @param {string} command
     * @param {string[]} [args]
     * @param {ExecOptions} [options]
     * @return {Promise<ExecResult>}
     */
    yarn(command, args = [], options = {}) {
      return exec("yarn", [...command.split(" "), ...args], options);
    },

    /**
     * Execute Mocha CLI
     *
     * @param {string[]} [args]
     * @param {ExecOptions} [options]
     * @return {Promise<ExecResult>}
     */
    mocha(args = [], options = {}) {
      // `yarn mocha` works, but format output differently
      // No more efforts here since we're plannig to drop Mocha
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
     * @param {ExecOptions} [options]
     * @return {Promise<ExecResult>}
     */
    rescript(command, args = [], options = {}) {
      const cliPath = path.join(import.meta.dirname, "../cli/rescript.js");
      return exec("node", [cliPath, command, ...args].filter(Boolean), options);
    },

    /**
     * `bsc` CLI
     *
     * @param {string[]} [args]
     * @param {ExecOptions} [options]
     * @return {Promise<ExecResult>}
     */
    bsc(args = [], options = {}) {
      return exec(bsc_exe, args, options);
    },

    /**
     * Execute ReScript `build` command directly
     *
     * @param {string[]} [args]
     * @param {ExecOptions} [options]
     * @return {Promise<ExecResult>}
     */
    execBuild(args = [], options = {}) {
      return exec(rescript_exe, ["build", ...args], options);
    },

    /**
     * Execute ReScript `clean` command directly
     *
     * @param {string[]} [args]
     * @param {ExecOptions} [options]
     * @return {Promise<ExecResult>}
     */
    execClean(args = [], options = {}) {
      return exec(rescript_exe, ["clean", ...args], options);
    },

    /**
     * Execute any binary or wrapper.
     * It should support Windows as well
     *
     * @param {string} bin
     * @param {string[]} [args]
     * @param {ExecOptions} [options]
     * @return {Promise<ExecResult>}
     */
    execBin(bin, args = [], options = {}) {
      return exec(bin, args, options);
    },
  };
}
