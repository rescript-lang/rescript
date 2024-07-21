const fs = require("node:fs");
const path = require("node:path");
const child_process = require("node:child_process");
const { rescript_exe } = require("#cli/bin_path");

const expectedFilePath = path.join(__dirname, "out.expected");

const updateTests = process.argv[2] === "update";

/**
 * @param {string} output
 * @return {string}
 */
function postProcessErrorOutput(output) {
  return output.trimEnd().replace(new RegExp(__dirname, "gi"), ".");
}

if (process.platform === "win32") {
  console.log("Skipping test on Windows");
  process.exit(0);
}

child_process.execSync(`${rescript_exe} clean`, { cwd: __dirname });

child_process.exec(rescript_exe, { cwd: __dirname }, (err, stdout, stderr) => {
  const actualErrorOutput = postProcessErrorOutput(stderr.toString());
  if (updateTests) {
    fs.writeFileSync(expectedFilePath, actualErrorOutput);
  } else {
    const expectedErrorOutput = postProcessErrorOutput(
      fs.readFileSync(expectedFilePath, { encoding: "utf-8" }),
    );
    if (expectedErrorOutput !== actualErrorOutput) {
      console.error(`The old and new error output aren't the same`);
      console.error("\n=== Old:");
      console.error(expectedErrorOutput);
      console.error("\n=== New:");
      console.error(actualErrorOutput);
      process.exit(1);
    }
  }
});
