const path = require("path");
const fs = require("fs");

const installMacLinuxBinary = (binary) => {
  const source = path.join(__dirname, binary);
  if (fs.existsSync(source)) {
    // mac and linux support extension-less executables
    // so just overwrite the shell script
    const target = path.join(__dirname, "rewatch");
    fs.renameSync(source, target);

    // The binary should be executable in the bundle, but just in case
    fs.chmodSync(target, 0777);
  } else {
    // assume we're in dev mode - nothing will break if the script
    // isn't overwritten, it will just be slower
  }
};

const installWindowsBinary = () => {
  const source = path.join(__dirname, "rewatch-windows.exe");
  if (fs.existsSync(source)) {
    const target = path.join(__dirname, "rewatch.exe");
    fs.renameSync(source, target);

    // windows scripts use a different file extension to executables
    // so we delete the script to make sure windows uses the exe now
    const windowsScript = path.join(__dirname, "rewatch.cmd");
    fs.unlinkSync(windowsScript);
  } else {
    // assume we're in dev mode - nothing will break if the script
    // isn't overwritten, it will just be slower
  }
};

switch (process.platform) {
  case "linux":
    if (process.arch === "arm64") {
      installMacLinuxBinary("rewatch-linux-arm64");
    } else {
      installMacLinuxBinary("rewatch-linux");
    }
    break;
  case "darwin":
    installMacLinuxBinary("rewatch-macos");
    break;
  case "win32":
    installWindowsBinary();
    break;
  default:
    // This won't break the installation because the shell script remains
    // but that script will throw an error in this case anyway
    console.warn(`No release available for "${process.platform}"`);
    process.exit(1);
}
