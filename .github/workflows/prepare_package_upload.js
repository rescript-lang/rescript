import * as fs from "node:fs";
import * as os from "node:os";

const packageSpec = JSON.parse(
  fs.readFileSync(new URL("../../package.json", import.meta.url), "utf-8"),
);

const { version } = packageSpec;

const commitHash = process.argv[2] || process.env.GITHUB_SHA;
const commitHashShort = commitHash.substring(0, 7);

// rescript
fs.renameSync("package.tgz", `rescript-${version}-${commitHashShort}.tgz`);

// @rescript/std
fs.renameSync(
  "packages/std/package.tgz",
  `rescript-std-${version}-${commitHashShort}.tgz`,
);

// @rescript/{target}
fs.renameSync(
  "packages/@rescript/linux-x64/package.tgz",
  `rescript-linux-x64-${version}-${commitHashShort}.tgz`,
);
fs.renameSync(
  "packages/@rescript/linux-arm64/package.tgz",
  `rescript-linux-arm64-${version}-${commitHashShort}.tgz`,
);
fs.renameSync(
  "packages/@rescript/darwin-x64/package.tgz",
  `rescript-darwin-x64-${version}-${commitHashShort}.tgz`,
);
fs.renameSync(
  "packages/@rescript/darwin-arm64/package.tgz",
  `rescript-darwin-arm64-${version}-${commitHashShort}.tgz`,
);
fs.renameSync(
  "packages/@rescript/win32-x64/package.tgz",
  `rescript-win32-x64-${version}-${commitHashShort}.tgz`,
);

// Pass information to subsequent GitHub actions
fs.appendFileSync(
  process.env.GITHUB_ENV,
  `rescript_version=${version}-${commitHashShort}${os.EOL}`,
);
