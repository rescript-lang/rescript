const fs = require("node:fs");
const os = require("node:os");

const { platformName } = require("#cli/bin_path");

// Pass artifactDirName to subsequent GitHub actions
fs.appendFileSync(
  process.env.GITHUB_ENV,
  `artifact_dir_name=${platformName}${os.EOL}`,
);
