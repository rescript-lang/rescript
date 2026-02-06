import child_process from "node:child_process";
import { bsc_exe, rescript_exe, runtimePath } from "./bins.mjs";

/**
 * Create a rescript CLI helper scoped to a working directory.
 * Provides methods to run rescript commands and spawn long-lived processes.
 *
 * @param {string} cwd - Working directory for spawned processes
 * @param {string} [otelEndpoint] - Optional OTEL endpoint for telemetry
 */
export function createRescriptCli(cwd, otelEndpoint) {
  const env = {
    ...process.env,
    RESCRIPT_BSC_EXE: bsc_exe,
    RESCRIPT_RUNTIME: runtimePath,
  };

  if (otelEndpoint) {
    env.OTEL_EXPORTER_OTLP_ENDPOINT = otelEndpoint;
  }

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
     * Spawn `rescript watch` and return a handle with control methods.
     * @param {string[]} args
     * @returns {{stop: () => Promise<void>, stdout: import("stream").Readable, stderr: import("stream").Readable, process: child_process.ChildProcess, waitForOutput: (pattern: RegExp, timeoutMs?: number) => Promise<string>}}
     */
    spawnWatch(args = []) {
      const controller = new AbortController();
      const { signal } = controller;

      const proc = child_process.spawn(rescript_exe, ["watch", ...args], {
        cwd,
        stdio: ["pipe", "pipe", "pipe"],
        env,
        signal,
      });

      // Suppress abort errors - they're expected when we stop the process
      proc.on("error", err => {
        if (err.name !== "AbortError") {
          throw err;
        }
      });

      // Collect stdout for pattern matching (progress messages).
      // stderr is collected separately for diagnostics (-vv debug output).
      let stdoutAll = "";
      let stdoutBuffer = "";
      let stderrAll = "";
      proc.stdout.on("data", chunk => {
        const text = chunk.toString();
        stdoutAll += text;
        stdoutBuffer += text;
      });
      proc.stderr.on("data", chunk => {
        stderrAll += chunk.toString();
      });

      // Track if process has exited
      let exited = false;
      proc.once("exit", () => {
        exited = true;
      });

      return {
        /**
         * Stop the watch process gracefully.
         * Closes stdin to signal EOF, which rewatch detects and exits.
         * Falls back to AbortController if the process doesn't exit
         * within the timeout.
         * @returns {Promise<void>}
         */
        async stop() {
          if (exited) return;

          // Signal graceful shutdown by closing stdin (EOF)
          proc.stdin.end();

          // Poll for graceful exit
          const deadline = Date.now() + 5000;
          while (!exited && Date.now() < deadline) {
            await new Promise(r => setTimeout(r, 100));
          }

          // Force kill if still running
          if (!exited) {
            await new Promise(resolve => {
              proc.once("exit", () => resolve());
              controller.abort();
            });
          }
        },
        get stdout() {
          return proc.stdout;
        },
        get stderr() {
          return proc.stderr;
        },
        process: proc,

        /**
         * Get all stdout received since process start (for debugging).
         */
        getAllOutput() {
          return stdoutAll;
        },

        /**
         * Get all stderr received since process start.
         * Contains -vv debug logging from rewatch.
         */
        getStderr() {
          return stderrAll;
        },

        /**
         * Wait for stdout matching a pattern.
         * After a successful match, the buffer is trimmed to start after the match,
         * so subsequent calls only match against new output.
         * Only matches against stdout since progress messages go there,
         * while -vv debug logging goes to stderr.
         * @param {RegExp} pattern
         * @param {number} timeoutMs
         * @returns {Promise<string>}
         */
        waitForOutput(pattern, timeoutMs = 10000) {
          return new Promise((resolve, reject) => {
            const timer = setTimeout(() => {
              proc.stdout.off("data", onData);
              reject(
                new Error(
                  `Timeout waiting for pattern ${pattern}.\nRemaining stdout buffer:\n${stdoutBuffer}\n\nAll stdout:\n${stdoutAll}\n\nStderr (-vv):\n${stderrAll}`,
                ),
              );
            }, timeoutMs);

            const tryMatch = () => {
              const match = stdoutBuffer.match(pattern);
              if (match) {
                clearTimeout(timer);
                proc.stdout.off("data", onData);
                const matchEnd = match.index + match[0].length;
                const matched = stdoutBuffer.slice(0, matchEnd);
                stdoutBuffer = stdoutBuffer.slice(matchEnd);
                resolve(matched);
              }
            };

            const onData = () => tryMatch();
            proc.stdout.on("data", onData);
            tryMatch(); // Check buffer immediately
          });
        },
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
        stdout: Buffer.concat(stdoutChunks)
          .toString("utf8")
          .replaceAll("\r\n", "\n"),
        stderr: Buffer.concat(stderrChunks)
          .toString("utf8")
          .replaceAll("\r\n", "\n"),
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
        stdout: Buffer.concat(stdoutChunks)
          .toString("utf8")
          .replaceAll("\r\n", "\n"),
        stderr: Buffer.concat(stderrChunks)
          .toString("utf8")
          .replaceAll("\r\n", "\n"),
      });
    });

    proc.stdin.write(input);
    proc.stdin.end();
  });
}
