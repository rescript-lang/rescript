// @ts-check

// Maps the current platform to the platform package (@rescript/<target>) that
// provides its compiler binaries. Kept free of side effects so it can be tested.

const supportedTargets = [
  "darwin-arm64",
  "darwin-x64",
  "linux-arm64",
  "linux-x64",
  "win32-x64",
];

// The first Windows 11 build. Windows 10 on ARM only emulates x86, not x64.
const firstWindows11Build = 22000;

/**
 * @param {string} platform `process.platform`
 * @param {string} arch `process.arch`
 * @param {string} osRelease `os.release()`, e.g. "10.0.22631" on Windows 11
 * @returns {string | undefined} the target, or undefined if unsupported
 */
function getTarget(platform, arch, osRelease) {
  // Windows 11 on ARM runs the x64 toolchain through its x64 emulation.
  const binaryArch =
    platform === "win32" &&
    arch === "arm64" &&
    windowsBuild(osRelease) >= firstWindows11Build
      ? "x64"
      : arch;
  const target = `${platform}-${binaryArch}`;
  return supportedTargets.includes(target) ? target : undefined;
}

/**
 * @param {string} osRelease
 */
function windowsBuild(osRelease) {
  const build = Number(osRelease.split(".")[2]);
  return Number.isInteger(build) ? build : 0;
}

exports.getTarget = getTarget;
