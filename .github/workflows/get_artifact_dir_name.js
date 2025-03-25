import * as fs from "node:fs";
import * as os from "node:os";

import { platformName } from "#cli/bins";

// Pass artifactDirName to subsequent GitHub actions
fs.appendFileSync(
  process.env.GITHUB_ENV,
  `artifact_dir_name=${platformName}${os.EOL}`,
);
