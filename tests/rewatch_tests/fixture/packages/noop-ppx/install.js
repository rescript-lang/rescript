const fs = require("fs");
const path = require("path");

const ppxJs = path.join(__dirname, "ppx.js");
const bin = path.join(__dirname, "bin");

if (process.platform === "win32") {
  // On Windows, create both:
  // 1. bin.cmd — the actual executable wrapper invoked by cmd /c
  // 2. bin — a dummy file so rewatch's path resolution (.exists() check) succeeds
  //
  // When the compiler runs "bin input output" via Sys.command (which uses cmd /c),
  // Windows automatically prefers bin.cmd over the extension-less bin.
  fs.writeFileSync(bin + ".cmd", `@node "%~dp0ppx.js" %*\r\n`);
  fs.writeFileSync(bin, "");
} else {
  // On Unix, create an executable Node.js script with a shebang
  fs.writeFileSync(
    bin,
    `#!/usr/bin/env node\n${fs.readFileSync(ppxJs, "utf8")}`,
  );
  fs.chmodSync(bin, 0o755);
}
