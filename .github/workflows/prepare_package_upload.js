import * as fs from "node:fs";
import * as os from "node:os";

const packageSpec = JSON.parse(
  fs.readFileSync(new URL("../../package.json", import.meta.url), "utf-8")
);

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
