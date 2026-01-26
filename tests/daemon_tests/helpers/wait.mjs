/**
 * Wait for a child process to exit.
 */
export function waitForProcessExit(child, timeoutMs = 5_000) {
  return new Promise((resolve, reject) => {
    const timer = setTimeout(
      () => reject(new Error("Process did not exit")),
      timeoutMs,
    );
    child.on("exit", () => {
      clearTimeout(timer);
      resolve();
    });
  });
}

/**
 * Wait for a watch process to exit and collect its output.
 * Use this when watch is expected to exit quickly (e.g., due to config error).
 * @param {{process: import("child_process").ChildProcess, stdout: import("stream").Readable, stderr: import("stream").Readable, stop: () => void}} watch
 * @param {number} timeoutMs
 * @returns {Promise<{status: number, stdout: string, stderr: string}>}
 */
export function collectProcessOutput(watch, timeoutMs = 10_000) {
  return new Promise((resolve, reject) => {
    const stdoutChunks = [];
    const stderrChunks = [];

    watch.stdout.on("data", chunk => stdoutChunks.push(chunk));
    watch.stderr.on("data", chunk => stderrChunks.push(chunk));

    const timer = setTimeout(() => {
      watch.stop();
      reject(new Error("Process did not exit within timeout"));
    }, timeoutMs);

    watch.process.once("close", exitCode => {
      clearTimeout(timer);
      resolve({
        status: exitCode ?? 1,
        stdout: Buffer.concat(stdoutChunks).toString("utf8"),
        stderr: Buffer.concat(stderrChunks).toString("utf8"),
      });
    });
  });
}
