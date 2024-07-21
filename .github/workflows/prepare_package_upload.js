const fs = require("node:fs");
const os = require("node:os");

const packageSpec = require("rescript/package.json");
const { version } = packageSpec;

const commitHash = process.argv[2] || process.env.GITHUB_SHA;
const commitHashShort = commitHash.substring(0, 7);

fs.renameSync(
  `rescript-${version}.tgz`,
  `rescript-${version}-${commitHashShort}.tgz`,
);
fs.renameSync(
  `packages/std/rescript-std-${version}.tgz`,
  `rescript-std-${version}-${commitHashShort}.tgz`,
);

// Pass information to subsequent GitHub actions
fs.appendFileSync(
  process.env.GITHUB_ENV,
  `rescript_version=${version}-${commitHashShort}${os.EOL}`,
);
